{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Digest.CRC32 as CRC
import Data.TMCR.HashIcons

import qualified Data.ByteString as BS

import Control.Arrow
import Control.Monad
import Data.Maybe
import Text.Read
import Text.Printf

import Data.Word

import Control.Concurrent (forkIO)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens
import Control.Lens.TH

import Miso
--import Miso.Html.Element
import Miso.String (toMisoString, fromMisoString)
-- import qualified Language.Javascript.JSaddle.Warp as JSaddle

staticRoot :: String
staticRoot = "https://ibot02.github.io/tmcr-hash-finder/static/"

data Filter = NoFilter | SpecificHash Word32 | HashIcons HashIcon HashIcon
    deriving (Eq, Ord, Show)

data Action = NoOp
            | SetPossibilities Int (Set Int)
            | SetFilter Filter
            | ComputePossibilites
            | ComputationDone [([Int],Word32)]
            -- ...
deriving instance Eq Action
deriving instance Ord Action
deriving instance Show Action

data Model = Model {
          _modelSettingsPossibilities :: [Set Int]
        , _modelFilter :: Filter
        , _modelCandidates :: [([Int], Word32)]
        , _modelComputationInProgress :: Bool
        } 

deriving instance Eq Model
deriving instance Ord Model
deriving instance Show Model

$(makeLenses ''Model)

data SelectableSetting = SettingFlag String | SettingDropdown [String] | SettingNumber String
        deriving (Eq, Ord, Show)

defaultLogicSettings :: [(SelectableSetting, Set Int)]
defaultLogicSettings = [
      flag "Keysanity"
    , flag "Shuffle elements into item pool"
    , flag' "Disable glitches" (Set.singleton 1)
    , flag "Obscure spots"
    , flag "Add available rupees to pool"
    , flag "Damaging items other than sword seen as weapons"
    , flag "Shuffle traps in itempool"
    , flag "Die in one hit if the timer runs out"
    , flag "Randomize Music"
    , flag' "Shuffle figurines into item pool" (Set.singleton 0)
    , dropdown ["No normal fusions", "Open fusion mode", "Vanilla Fusions"]
    , dropdown ["Allow fusion skips", "Instant fusion skips", "No fusion skips"]
    , dropdown ["Ped Open/Regular DHC", "Ped Finish/No DHC", "Open DHC", "Open DHC + Ped Items"]
    , number "Figurine count"
    , number "Extra figurines"
    , dropdown ["No Sword Ped", "Smith Sword Ped", "White Sword Ped", "Red Sword Ped", "Blue Sword Ped", "Four Sword Ped"]
    , dropdown ["4 Element Ped", "3 Element Ped", "2 Element Ped", "1 Element Ped", "No Element Ped"]
    ] where
        flag name = flag' name (Set.fromList [0,1])
        flag' name options = (SettingFlag name, options)
        dropdown options = (SettingDropdown options, Set.fromList [0..length options - 1])
        number name = (SettingNumber name, Set.singleton 0)

main =
    startApp App {
          initialAction = NoOp
        , model = initialModel
        , update = updateModel
        , Miso.view = viewModel
        , events = defaultEvents
        , subs = []
        , mountPoint = Nothing
        , logLevel = Off
        }

initialModel :: Model
initialModel = Model (fmap snd defaultLogicSettings) NoFilter [] False

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp = fromTransition $ return ()
updateModel (SetPossibilities index possibilities) = fromTransition $
        modelSettingsPossibilities . ix index .= possibilities
updateModel (SetFilter filter) = fromTransition $
        modelFilter .= filter
updateModel ComputePossibilites = fromTransition $ do
        possibilities <- use modelSettingsPossibilities
        scheduleSub $ computeCandidates possibilities
        modelComputationInProgress .= True
updateModel (ComputationDone candidates) = fromTransition $ do
        modelCandidates .= candidates
        modelComputationInProgress .= False

viewModel :: Model -> View Action
viewModel model = div_ [] [
      possibilitiesDialog model
    , filterAndComputeDialog model
    , output model
    , link_ [rel_ "stylesheet",
             href_ (toMisoString staticRoot <> "style.css")]
    ]

possibilitiesDialog :: Model -> View Action
possibilitiesDialog model = div_ [class_ "possibilities"] $ imap (\i ((s, _),p) -> viewPossibilityChoice i (model ^. modelComputationInProgress) s p) $ zip defaultLogicSettings (model ^. modelSettingsPossibilities)

viewPossibilityChoice :: Int -> Bool -> SelectableSetting -> Set Int -> View Action
viewPossibilityChoice i d (SettingFlag n) p = span_ [class_ ("flag flag-" <> toMisoString (show i))] [label_ [for_ (toMisoString ("flag-select-" ++ show i))] [text (toMisoString n)], selection (SetPossibilities i) [id_ (toMisoString $ "flag-select-" ++ show i), disabled_ d] p [("On", Set.singleton 1), ("Unknown", Set.fromList [0,1]), ("Off", Set.singleton 0)]]
viewPossibilityChoice i d (SettingDropdown opts) p = span_ [class_ ("dropdown dropdown-" <> toMisoString (show i))] [selection (SetPossibilities i) [id_ (toMisoString $ "dropdown-select-" ++ show i), disabled_ d] p (("Unknown", Set.fromList [0..length opts - 1]):imap (\i o -> (o, Set.singleton i)) opts)]
viewPossibilityChoice i d (SettingNumber n) p = span_ [class_ ("number number-" <> toMisoString (show i))] [text (toMisoString n), input_ [type_ "number", name_ (toMisoString n), onChange (maybe NoOp (SetPossibilities i . Set.singleton) . readMaybe . fromMisoString), value_ (toMisoString $ show $ fromMaybe 0 $ p ^? to Set.toList . traverse)]]

selection :: (Eq a) => (a -> Action) -> [Attribute Action] -> a -> [(String, a)] -> View Action
selection f attr selected options = select_ (onChange (\s -> f $ snd $ options !! read (fromMisoString s)):attr) $ imap (\i (o, v) -> option_ [selected_ (v == selected), value_ (toMisoString (show i))] [text (toMisoString o)]) options

filterAndComputeDialog :: Model -> View Action
filterAndComputeDialog model = div_ [class_ "filter-and-compute"] [
      input_ [type_ "button", value_ (toMisoString (printf "Compute All %d Options" (productOf (modelSettingsPossibilities . traverse . to Set.size) model) :: String)), onClick ComputePossibilites, disabled_ (model ^. modelComputationInProgress)]
    , div_ [class_ "filter"] (selection SetFilter [id_ "filter-selection"] (model ^. modelFilter . to forgetSpecificsFilter) [("Show All Possibilites", NoFilter), ("Match Hash:", SpecificHash 0), ("Match Icons", HashIcons minBound minBound)] : case model ^. modelFilter of
        NoFilter -> []
        SpecificHash h -> [hexInput (SetFilter . SpecificHash) h]
        HashIcons i1 i2 -> [hashIconInput (SetFilter . flip HashIcons i2) i1, hashIconInput (SetFilter . HashIcons i1) i2])
    ] where
        forgetSpecificsFilter NoFilter = NoFilter
        forgetSpecificsFilter (SpecificHash _) = SpecificHash 0
        forgetSpecificsFilter (HashIcons _ _) = HashIcons minBound minBound


hexInput :: (Word32 -> Action) -> Word32 -> View Action
hexInput f v = input_ [pattern_ "[0-9a-fA-F]{0,8}", value_ (toMisoString (printf "%08lX" v :: String)), onChange (f . fromMaybe 0 . readMaybe . ("0x" <>) . fromMisoString)]

hashIconInput :: (HashIcon -> Action) -> HashIcon -> View Action
hashIconInput f v = select_ [onChange (f . fromMaybe minBound . readMaybe . fromMisoString)] $ fmap (\ic -> option_ [value_ (toMisoString $ show ic), selected_ (ic == v), style_ (Map.fromList [("width", "16px"), ("height", "16px")])] [text $ toMisoString $ show ic]) [minBound..maxBound]

viewHashIcon :: HashIcon -> View a
viewHashIcon i = div_ [style_ (Map.fromList[("background-image", "url(" <> toMisoString staticRoot <> "img/hashicons.png)"),("background-position-y", toMisoString $ show (-16 * fromEnum i) <> "px"),("width","16px"), ("height", "16px"), ("overflow", "hidden")]), title_ (toMisoString $ show i)] []

output :: Model -> View Action
output model = div_ [class_ "output"] $ model ^.. modelCandidates . traverse . filtered (matchesFilter (model ^. modelFilter)) . to displayCandidate where
        matchesFilter NoFilter _ = True
        matchesFilter (SpecificHash h) (_, h') = h == h'
        matchesFilter (HashIcons i1 i2) (_, h) = (i1, i2) == fromSettingsHash h
        displayCandidate (options, h) = details_ [class_ "candidate"] $ [
              summary_ [] [
                  displayHash h
                , displayIcons h
                ]
            , div_ [class_ "possibilities"] (displayOptions options)
            ]
        displayOptions o = imap displayOption $ filter (\((_, s),_) -> Set.size s > 1) $ zip (zip o (model ^. modelSettingsPossibilities)) defaultLogicSettings
        displayOption i ((selected,_),(choice,_)) = viewPossibilityChoice i True choice $ Set.singleton selected
        displayHash h = span_ [class_ "hash"] [text $ toMisoString (printf "%08lX" h :: String)]
        displayIcons (fromSettingsHash -> (i1, i2)) = span_ [class_ "hash-icons"] [viewHashIcon i1, viewHashIcon i2]


computeCandidates :: [Set Int] -> Sub Action
computeCandidates options callback = void $ forkIO $ callback $ ComputationDone $ fmap (\o -> (o, hash o)) $ traverse Set.toList options where
        hash o = CRC.digest $ BS.pack $ fmap toEnum o
