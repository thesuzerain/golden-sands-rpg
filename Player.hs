module Player where
import System.Random
import Utility


data PlayerData = PlayerData
    { name :: [Char]
      , playerClass :: Type
      , hp :: Integer,
      maxHP :: Integer,
      ap :: Integer,
      maxAP :: Integer,
      level :: Integer,
      attack :: Integer,
      defense :: Integer,
      intelligence :: Integer,
      dexterity :: Integer,
      currency :: Integer,
      experience :: Integer,
      items :: [(ItemName,Integer)]
    }

data Type = Melee | Range | Magic | NullType deriving (Show, Eq, Enum)
data ItemName = Null | HPotion | APotion deriving (Show, Eq, Enum)
data Stat = Attack | Defense | Intelligence | Dexterity deriving (Show, Eq, Enum)

getClass :: IO Type
getClass = do
    val <- (humanInt 2)
    case val of
        0 -> return Melee
        1 -> return Range
        2 -> return Magic

         --fmap (toEnum . read) getLine

getNewAP typ lvl = do
    putStrLn $ show (4+lvl)
    return(4+lvl)

getNewHP :: Type -> Integer -> IO Integer
getNewHP typ lvl
    | typ == Melee = do
        m <- randomRIO (-3,3)
        return (m + (30*lvl))
    | typ == Range = do
        m <- randomRIO(-3,3)
        return (m + (24*lvl))
    | typ == Magic = do
        m <- randomRIO(-3,3)
        return (m + (18*lvl))
    | otherwise = return(1)

getNewStat :: Type -> Stat -> Integer -> IO Integer
getNewStat typ stat lvl = (getStatFromTuple (getStatTuple typ) lvl stat)

-- (Attack, Defense, Intelligent, Dexterity)
getStatTuple typ
    | typ == Melee = (8,8,2,2)
    | typ == Range = (4,5,3,8)
    | typ == Magic = (3,2,9,6)
    | otherwise = (2,2,2,2)


getStatFromTuple (a,b,c,d) lvl stat
    | stat == Attack = do
        m <- randomRIO(-1,1)
        return (m + (a*lvl))
    | stat == Defense = do
        m <- randomRIO(-1,1)
        return (m + (b*lvl))
    | stat == Intelligence = do
        m <- randomRIO(-1,1)
        return (m + (c*lvl))
    | stat == Dexterity = do
        m <- randomRIO(-1,1)
        return (m + (d*lvl))
