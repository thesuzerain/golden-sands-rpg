-- CPSC 312 - Wyatt and Josh - Gladiator RPG
--module RPG where
import System.Random
import System.Console.ANSI
import Player
import Utility
import Shop
import BattleDT
import Battle
-- To run it, try:
-- ghci
-- :load RPG




 --  Melee <- Range <- Magic <- Melee
 -- Magic: Fire WaterGrass or w/e
 -- HP Potion, MP Potion (limited), Scrolls <- these are bought with money won
 --
 --  An enemy will have:
 --     Melee 1-10
 --     Range 1-10
 --     Magic 1-10
 --     maybe a couple items
 --     fighting style
 --     HP/defense
 --

 -- X is strong against Y
--strongAgainst :: Type -> Type
--strongAgainst Range = Melee
--strongAgainst Magic = Range
--strongAgainst Melee = Magic


data Move = Rock | Paper | Scissors deriving (Show, Eq, Enum)

instance Ord Move where
  (<=) x y = x == y || (x,y) `elem` [(Rock,Paper),(Paper,Scissors),(Scissors,Rock)]

humanSelect :: IO Move
humanSelect = fmap (toEnum . read) getLine

computerSelect :: IO Move
computerSelect = fmap toEnum (randomRIO (0,2))

resultString :: Ordering -> String
resultString y = case y of
  LT -> "Player Wins"
  EQ -> "Draw!"
  GT -> "Computer Wins"


main :: IO()
main = do
  doRegisterText
  putStrLn $ colourize $ "Please enter your name: "
  doEndText
  name <- getLine
  doRegisterText
  putStrLn $ colourize $  "Welcome, " ++ show name ++ "! "
  putStrLn $ colourize $ "By typing in /.b0/.0, /.b1/.0, or /.b2/.0, please select your character's class from the options below:"
  putStrLn $ colourize $ "~ /.rMELEE/.0 ~ (/.b0/.0) - A warrior based class with high attack and defense, but very slow, and no magic."
  putStrLn $ colourize $ "~ /.gRANGE/.0 ~ (/.b1/.0) - An archer based class with high speed and often able to evade, but low attack and defense. "
  putStrLn $ colourize $ "~ /.mMAGIC/.0 ~ (/.b2/.0) - A wizard based class with moderate speed, high attack, but very low defense."
  putStrLn $ colourize $ "By typing in /.b0/.0, /.b1/.0, or /.b2/.0, please select your character's class!"
  doEndText
  playerClass <- getClass
  hp <- (getNewHP playerClass 1)
  let maxHP = hp
  let ap = 5
  let maxAP = 5
  let level = 1
  attack <-  (getNewStat playerClass Attack 1)
  defense <-  (getNewStat playerClass Defense 1)
  intelligence <-  (getNewStat playerClass Intelligence 1)
  dexterity <-  (getNewStat playerClass Dexterity 1)
  let currency = 300
  let experience = 0
  let items = []
  run (PlayerData name playerClass hp maxHP ap maxAP level attack defense intelligence dexterity currency experience items)

doRegisterText :: IO()
doRegisterText = do

  clearScreen
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn $ colourize $ "Welcome to the /.yGolden Sands Arena/.0! "

doEndText :: IO()
doEndText = do
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="

displayPlayerData :: PlayerData -> IO Bool
displayPlayerData player = do
  clearScreen
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn $ colourize $ "/.r"++ show (name player)++ "/.0 - - - - - /.b"++ show (playerClass player)++ "/.0 "
  putStrLn $ colourize $ "EXP:/.r"++ show (experience player)++ "/.0 - - - - - Currency: /.y"++ show (currency player)++ "/.0 "
  putStrLn $ colourize $ "HP:/.b"++ show (maxHP player)++ "/.0 - - - - - AP: /.y"++ show (maxAP player)++ "/.0 "
  putStrLn $ colourize $ "Attack:/.b"++ show (attack player)++ "/.0 - - - - - Defense: /.y"++ show (defense player)++ "/.0 "
  putStrLn $ colourize $ "Intelligence:/.b"++ show (intelligence player)++ "/.0 - - - - - Dexterity: /.y"++ show (dexterity player)++ "/.0 "
  putStrLn $ " "
  putStrLn $ colourize $ (buildItemStringForPlayer (items player) (length (items player)))
  putStrLn $ " "
  putStrLn $ colourize $ "Enter (/.b0/.0) to continue..."
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  boop <- getLine
  return(False)

run :: PlayerData -> IO()
run player = do
  clearScreen

  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  -- Outside Battle

  -- we set some conditions for battle (ie, shop, choose character)

  -- battle player opponent

  putStrLn $ "Welcome to the Golden Sands Arena!"
  putStrLn $ colourize "Select an action: \"/.rBattle/.0\" (/.b0/.0), /.rShop/.0\" (/.b1/.0), \"/.rView Profile/.0\" (/.b2/.0), \"/.rQuit/.0\" (/.b3/.0) "
--  putStrLn "Select an action: \"/.rBattle/.0\" (\.b1\.0), /.rShop/.0\" (\.b2\.0), \"/.rView Profile\" (\.b3\.0), \"/.rQuit/.0\" (\.b4\.0)"
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="

  humanMove <- humanInt 3
  computerMove <- computerSelect
  tempRand <- randomRIO(-1,1)
  randomOpp <- (getRandomOpponent (max 1 ((level player)+tempRand)))
  (newPlayer,termiTemp) <-  case humanMove of
                            0 -> battleLoopInit player randomOpp
                            1 -> startShop player
                            2 -> do return(player,(displayPlayerData player))
                            3 -> do return(player,getIOBool True)
  terminateLoop <- termiTemp

  if terminateLoop == True then putStrLn ("Thank you for playing!") else run (healPlayer newPlayer)



getRandomOpponent level = do
  let listOfNames=["John","Josh","Alex","Steve","Barney","Thomas","Tom","Wyatt","David","Gurney","Alexis","Alexes","Jane","Debra","Lottie","Haley","Sarah","Mckenna","Stephen"]
  rintforname <- randomRIO(0,(length listOfNames) - 1)
  let listOfTypes=[Melee,Range,Magic]
  rintfortype <- randomRIO(0,(length listOfTypes) - 1)
  hp <- (getNewHP (listOfTypes !! rintfortype) level)
  ap <- (getNewAP (listOfTypes !! rintfortype) level)
  currency <- randomRIO(1,20*level)
  expe <- randomRIO(1,10*level)
  atk <- (getNewStat (listOfTypes !! rintfortype) Attack level)
  def <- (getNewStat (listOfTypes !! rintfortype) Defense level)
  int <- (getNewStat (listOfTypes !! rintfortype) Intelligence level)
  dex <- (getNewStat (listOfTypes !! rintfortype) Dexterity level)
  return(PlayerData (listOfNames !! rintforname) (listOfTypes !! rintfortype) hp hp ap ap level atk def int dex currency expe [])

battle :: Integer -> Integer -> IO()
battle player opponent = do
  let exitVal = 1





  if exitVal == 1
    then return()
    else do battle player opponent
--  functionBreak val player opponent
{--
functionBreak :: Integer -> Integer -> Integer -> IO()
functionBreak val player opponent
  | val >= 1 = battle player opponent
  | val == 1 = main
--}

testAI = do
  opp1 <-  (getRandomOpponent 1)
  opp2 <- (getRandomOpponent 1)
  putStrLn $ show (Player.ap opp1)
  putStrLn $ show (Player.ap opp2)
  return(getAIFromTree opp1 opp2)
