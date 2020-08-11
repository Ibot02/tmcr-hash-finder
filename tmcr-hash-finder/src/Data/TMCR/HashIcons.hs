module Data.TMCR.HashIcons (HashIcon(..), fromSettingsHash) where

import Data.Bits
import Data.Word

fromSettingsHash :: Word32 -> (HashIcon, HashIcon)
fromSettingsHash n = (toEnum $ fromIntegral (63 .&. shiftR n 8), toEnum $ fromIntegral (63 .&. shiftR n 16))

data HashIcon =
          SmithsSword
        | GreenSword
        | RedSword
        | BlueSword
        | Bomb
        | RemoteBomb
        | Bow
        | Boomerang
        | MagicalBoomerang
        | SmallShield
        | MirrorShield
        | LitLantern
        | GustJar
        | Cane
        | Mitts
        | Cape
        | Boots
        | Ocarina
        | EarthElement
        | FireElement
        | WaterElement
        | WindElement
        | SmallKey
        | Map
        | BigKey
        | Compass
        | RedBook
        | BlueBook
        | LonLonKey
        | GraveyardKey
        | Flippers
        | Zeffa
        | HeartContainer
        | NayrusPearl
        | FaroresPearl
        | DinsPearl
        | BrokenPicoriBlade
        | Shells
        | KinstoneBag
        | Butterfly
        | RefillHeart
        | GreenRupee
        | BigGreenRupee
        | BlueRupee
        | BigBlueRupee
        | RedRupee
        | BigRedRupee
        | GreenKinstone
        | BlueKinstone
        | RedKinstone
        | GoldKinstone
        | Brioche
        | Croissant
        | SliceOfPie
        | SliceOfCake
        | RedOcto
        | BlueOcto
        | Cucco
        | GoldCucco
        | Skull
        | RedEgg
        | BlueEgg
        | GreenEgg
        | YellowEgg
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
