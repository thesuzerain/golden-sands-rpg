module Battle where

import Player
import Utility
import System.Console.ANSI
import System.Random
import Data.Time.Clock



battleLoopInit player opponent =
  battleLoop player opponent 0 0 0 Null 0 0 Null

humanIntWithAPCheck :: Integer -> Integer -> IO Integer
humanIntWithAPCheck limit currAP = do
    response <- humanInt limit
    if (apCostOfAMove response) <= currAP then do return(response)
      else do
        putStrLn $ "Not enough AP. Please select a different option."
        val <- humanIntWithAPCheck limit currAP
        return(val)


battleLoop :: PlayerData -> PlayerData -> Integer -> Integer -> Integer -> ItemName -> Integer -> Integer -> ItemName -> IO (PlayerData,IO Bool)
battleLoop player opponent frame c1 d1 i1 c2 d2 i2 = do
  putStrLn "boing"

  doBattleText player opponent
  putStrLn $ " "
  doLastAttackText player opponent c1 d1 i1 c2 d2 i2
  putStrLn $ " "
  let levelUp = if frame==10 && (willLevelUpTo player opponent) == (level player) then 0 else (willLevelUpTo player opponent)
  doActionText frame player levelUp (experience opponent) (currency opponent)
  doEndLinesText
  response <- humanIntWithAPCheck (if frame == 1 && (level player)<5 then 1 else if frame == 1 && (level player)<10 then 2 else if frame == 2 then (fromIntegral (length (items player))) else 3) (if frame == 1 then (ap player) else 100000000)
  putStrLn "hoog"
  putStrLn $ show response
  let oppresponse = 0
--  let (a,b,c,d,e,f,g,h) = if (dexterity player)>(dexterity opponent) then (responseFrame (player,opponent,c1,d1,i1,c2,d2,i2) frame response False)  else (responseFrame (player,opponent,c1,d1,i1,c2,d2,i2) frame oppresponse True)
--  let (aF,bF,cF,dF,eF,fF,gF,hF) = if (dexterity player)>(dexterity opponent) then (responseFrame (a,b,c,d,e,f,g,h) frame oppresponse True)  else (responseFrame (a,b,c,d,e,f,g,h) frame response False)
  (a,b,c,d,e,f,g,h) <- if (dexterity player)>(dexterity opponent) then (responseFrame (player,opponent,c1,d1,i1,c2,d2,i2) frame response False)  else (responseFrame (player,opponent,c1,d1,i1,c2,d2,i2) frame oppresponse True)
  (aF,bF,cF,dF,eF,fF,gF,hF) <- if (dexterity player)>(dexterity opponent) then (responseFrame (a,b,c,d,e,f,g,h) frame oppresponse True)  else (responseFrame (a,b,c,d,e,f,g,h) frame response False)
  let finalPlayer = if frame==10 then (getNewEXPPlayer aF bF False) else if frame == 11 then (getNewEXPPlayer aF bF True) else do aF
  putStrLn $ show frame
  let newFrame = (fromIntegral (getNewFrame (fromIntegral frame) finalPlayer bF (fromIntegral response)))
  if newFrame == 12
    then do
      return(finalPlayer,getIOBool False)
    else do
      battleLoop finalPlayer bF newFrame cF dF eF fF gF hF

-- TODO: make AP visible on screen


willLevelUpTo :: PlayerData -> PlayerData -> Integer
willLevelUpTo player opponent = if (experience player) > ((level player) * 10) then (level player) + 1 else (level player)

getNewEXPPlayer ::  PlayerData -> PlayerData -> Bool -> PlayerData
getNewEXPPlayer attacker opponent didLose = (PlayerData (name attacker) (playerClass attacker) (hp attacker) (maxHP attacker) (ap attacker) (maxAP attacker) newLvl newAtk newDef newInt newDex newCurrency newEXP (items attacker))
  where
    newLvl = (willLevelUpTo attacker opponent)
    newEXP = if newLvl == (level attacker) then ((experience opponent)+(experience attacker)) else 0
    newCurrency = if didLose then (currency attacker) - (currency opponent) else (currency attacker) + (currency opponent)
    (a,b,c,d) = getStatTuple (playerClass attacker)
    newAtk = if newLvl == (level attacker) then (attack attacker) else (attack attacker) + a
    newDef = if newLvl == (level attacker) then (defense attacker) else (defense attacker) + b
    newInt = if newLvl == (level attacker) then (intelligence attacker) else (intelligence attacker) + c
    newDex = if newLvl == (level attacker) then (dexterity attacker) else (dexterity attacker) + d

responseFrame :: (PlayerData, PlayerData, Integer, Integer, ItemName, Integer, Integer, ItemName) -> Integer -> Integer -> Bool -> IO (PlayerData, PlayerData, Integer, Integer, ItemName, Integer, Integer, ItemName)
responseFrame (player,opponent,c1,d1,i1,c2,d2,i2) frame response isOpponent
  | frame == 0 = do return ((player,opponent,0,0,Null,0,0,Null))
  | frame == 1 || (frame == 2 && isOpponent && response /= 0)= do
    rVal <- randomRIO(0,2)
    return (attackFrameHelper (player,opponent,c1,d1,i1,c2,d2,i2) frame response isOpponent rVal)
  | (frame == 2 && (not isOpponent)) = do
     if response == 0 then do return (player,opponent,c1,d1,i1,c2,d2,i2) else do return (potionFrameHelper (player,opponent,c1,d1,i1,c2,d2,i2) frame (fromIntegral (response - 1)))
  | otherwise = do return((player,opponent,c1,d1,i1,c2,d2,i2))

potionFrameHelper :: (PlayerData, PlayerData, Integer, Integer, ItemName, Integer, Integer, ItemName) -> Integer -> Integer -> (PlayerData, PlayerData, Integer, Integer, ItemName, Integer, Integer, ItemName)
potionFrameHelper (player,opponent,c1,d1,i1,c2,d2,i2) frame response =
  (res2,opponent,c1,res1,(fst ((items player) !! (fromIntegral (response)))),c2,d2,i2)
  where (res1,res2) = updateForPotion player opponent response

updateForPotion :: PlayerData -> PlayerData -> Integer  -> (Integer, PlayerData)
updateForPotion attacker enemy attackNumber = (d1, (PlayerData (name attacker) (playerClass attacker) retHP (maxHP attacker) retAP (maxAP attacker) (level attacker) (attack attacker) (defense attacker) (intelligence attacker) (dexterity attacker) (currency attacker) (experience attacker) retItems))
    where
      retHP = if (fst ((items attacker) !! (fromIntegral attackNumber))) == HPotion then (min ((hp attacker) + 20) (maxHP attacker)) else (hp attacker)
      retAP = if (fst ((items attacker) !! (fromIntegral attackNumber))) == APotion then (min ((ap attacker) + 10) (maxAP attacker)) else (ap attacker)
      d1 = if (fst ((items attacker) !! (fromIntegral attackNumber))) == HPotion then (retHP - (hp attacker)) else (retAP - (ap attacker))
      retItems = delItem (items attacker) (fromIntegral attackNumber)

delItem :: [(ItemName, Integer)] -> Integer -> [(ItemName, Integer)]
delItem items attackNumber
  | attackNumber == 0 && (snd (head items)) == 1 = (tail items)
  | attackNumber == 0 = ((fst (head items)), ((snd (head items)-1))):(tail items)
  | otherwise = (head items):(delItem (tail items) (attackNumber - 1))


attackFrameHelper (player,opponent,c1,d1,i1,c2,d2,i2) frame response isOpponent randomVal =
  if isOpponent then (res3,res2,c1,d1,i1,c2,res1,i2) else (res2,res3,c1,res1,i1,c2,d2,i2)
  where (res1,res2,res3) = if isOpponent then updateForAttack opponent player response randomVal else updateForAttack player opponent response randomVal

updateForAttack attacker enemy attackNumber randomVal = (d1, (PlayerData (name attacker) (playerClass attacker) (hp attacker) (maxHP attacker) retAP (maxAP attacker) (level attacker) (attack attacker) (defense attacker) (intelligence attacker) (dexterity attacker) (currency attacker) (experience attacker) (items attacker)), (PlayerData (name enemy) (playerClass enemy) retHP (maxHP enemy) (ap enemy) (maxAP enemy) (level enemy) (attack enemy) (defense enemy) (intelligence enemy) (dexterity enemy) (currency enemy) (experience enemy) (items enemy)))
  where
    d1 = (baseDamageFromToFor attacker enemy attackNumber) + randomVal
    retHP = (hp enemy) - d1
    retAP = (max 0 ((ap attacker) - (apCostOfAMove attackNumber)))

apCostOfAMove n
  | n == 0 = 0
  | n == 1 = 3
  | n == 2 = 3
  | n == 3 = 10
  | otherwise = 0

baseDamageFromToFor :: PlayerData -> PlayerData -> Integer -> Integer
baseDamageFromToFor attacker enemy select
-- | (rotateClassFor (playerClass attacker) attackNumber)==Melee = ((fromIntegral (attack attacker)/(fromIntegral (defense enemy)))) + attackNumber*2
  | (rotateClassFor (playerClass attacker) attackNumber)==Melee = ((attack attacker) `div` (defense enemy)) + 2*attackNumber^2
  | (rotateClassFor (playerClass attacker) attackNumber)==Range = ((intelligence attacker) `div` (defense enemy))  + 2*attackNumber^2
  | (rotateClassFor (playerClass attacker) attackNumber)==Magic = ((attack attacker) `div` (dexterity enemy))  + 2*attackNumber^2
  | otherwise = 1
  where attackNumber = select + 1

getNewFrame :: Int -> PlayerData -> PlayerData -> Int -> Int
getNewFrame frame player opponent r
  | frame == 10 = 12
  | frame == 11 = 12
  | (hp opponent) < 1 = 10
  | (hp player) < 1 = 11
  | frame == 0 = if r == 0 then 1 else if r == 1 then 2 else if r == 2 then 11 else 0
  | frame == 1 = 0
  | otherwise = 0

{--getNewFrame2 :: Integer -> PlayerData -> IO Integer -> IO Integer
getNewFrame2 frame player r
  | frame == 0 = do
     response <- r
     if response == 0 then return(1) else if response == 1 then return(2) else if response == 2 then return(3) else return(0)
  | otherwise = do return(1)
--}

rotateClassFor classType val
  | classType == Melee = if val==3 then Range else Melee
  | classType == Range = if val==3 then Magic else Range
  | classType == Magic = if val==3 then Melee else Magic
  | otherwise = Magic

doBattleText player opponent = do
  clearScreen
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn $ colourize $ (name player) ++ (thisManySpaces (35 - (length (name player))) ++ (name opponent)) ++ " "
  putStrLn $ colourize $ "HP : " ++ (getHPBar (hp player) (maxHP player)) ++ "          " ++ "HP : " ++ (getHPBarOpp (hp opponent) (maxHP opponent ))  ++ " "
  let startAry = "Level " ++ show (level player) ++ " " ++ show (playerClass player)  ++ " "
  putStrLn $ colourize $ startAry ++ (thisManySpaces (35 - (length (startAry)))) ++ "Level " ++ show (level opponent) ++ " " ++ show (playerClass opponent)  ++ " "


doLastAttackText :: PlayerData -> PlayerData -> Integer -> Integer -> ItemName -> Integer -> Integer -> ItemName -> IO()
doLastAttackText player opponent c1 d1 i1 c2 d2 i2 = do
  -- 0 1 Null 0 6 Null
  if d2 == 0 && d2 == 0 && i2 == Null && i1 == Null
    then  do
      putStrLn $ ""

    else
      if i1 == Null
        then do
          putStrLn $ colourize $ "Hit " ++ (name opponent) ++ " with a " ++ show (rotateClassFor (playerClass player) c1) ++ " attack for " ++ show d1 ++ " damage!"
          doLastAttack2 player opponent c1 d1 i1 c2 d2 i2

        else do
          putStrLn $ colourize $ "Used the " ++ show i1 ++ " for "++show d1++"!"
          doLastAttack2 player opponent c1 d1 i1 c2 d2 i2


doLastAttack2 player opponent c1 d1 i1 c2 d2 i2 = do
  if i2 == Null
    then
      putStrLn $ colourize $ (name opponent) ++ " hit you  with a " ++ show (rotateClassFor (playerClass opponent) c1) ++ " attack for " ++ show d2 ++ " damage!"
    else
      putStrLn $ colourize $ (name opponent) ++ " used the " ++ show i1 ++ "!"


doActionText :: Integer -> PlayerData -> Integer -> Integer -> Integer -> IO()
doActionText frame player levelUp newExp newCurrency
  | frame == 0 = do
      putStrLn $ " "
      putStrLn $ "Choose an action : "
      putStrLn $ colourize $ "\"/.rFight/.0\" (/.b0/.0), \"/.rItems/.0\" (/.b1/.0), \"/.rForfeit/.0\" (/.b2/.0)"
  | frame == 1 = do
      putStrLn $ " "
--      let peep = if bool then "sick" else "doom"
      putStrLn $ colourize $ "Choose an attack (/.b"++ show (ap player) ++ "/.0//.b" ++ show (maxAP player) ++ "/.0) : "
      -- Basic attack, low chance to miss
      let atk1 = if (playerClass player) == Melee then "Sword Slash (/.r0AP/.0)" else if (playerClass player) == Range then "Longbow (/.r0AP/.0)" else "Fireball (/.r0AP/.0)"
      let atk2 = if (playerClass player) == Melee then "Morningstar (/.r3AP/.0)" else if (playerClass player) == Range then "Crossbow (/.r3AP/.0)"  else "Lightningbolt (/.r3AP/.0)"
      let atk3 = if (playerClass player) == Melee then "Phantom Punch (/.r3AP/.0)"  else if (playerClass player) == Range then "Sword Throw (/.r3AP/.0)" else "Meteor Strike (/.r3AP/.0)"
      let atk4 = if (playerClass player) == Melee then "Herculean Fist (/.r10AP/.0)" else if (playerClass player) == Range then "Tankbuster (/.r10AP/.0)" else "Divine Retribution (/.r10AP/.0)"

      let buildStr = "\""++atk1++"\" (/.b0/.0), \""++atk2++"\" (/.b1/.0)"
      let buildStr1 = if ((level player) >= 5) then buildStr ++ ", \""++atk3++"\" (/.b2/.0)" else buildStr
      let buildStr2 = if ((level player) >= 10) then buildStr1 ++ ", \""++atk4++"\" (/.b2/.0)" else buildStr1
      putStrLn $ colourize $ buildStr2

  | frame == 2 = do
      putStrLn $ colourize $ "Select an Item to use or press (/.b0/.0) to cancel: "
      putStrLn $ colourize $ (buildItemString (items player) 1)
  | frame == 3 = putStrLn $ ""
  | frame == 4 = putStrLn $ ""
  | frame == 5 = putStrLn $ ""
  | frame == 10 = do
    putStrLn $ colourize $"You won the battle! Gained /.r"++ show newExp ++"/.0 EXP! Gained /.y"++ show newCurrency ++"/.0G!"
    if levelUp == 0 then  return() else putStrLn $ colourize $ "Level Up! You are now Level /.r" ++ show levelUp ++ "/.0! Congratulations!"
    putStrLn $ colourize $ "Enter (/.b0/.0) to continue!"
  | frame == 11 = putStrLn $ colourize $ "You lost the battle. Lost /.y"++show newCurrency ++"/.0G."
  | otherwise = putStrLn $ ""

doEndLinesText = do
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="

buildItemString items size
  | items == [] = ""
  | otherwise =  (buildItemString (tail items) (size + 1)) ++"(/.r"++show (fst (head items))++" x"++show (snd (head items))++"/.0) (/.b"++show size++"/.0)   "


buildItemStringForPlayer items size
    | items == [] = ""
    | otherwise =  (buildItemString (tail items) (size + 1)) ++"(/.r"++show (fst (head items))++" x"++show (snd (head items))++"/.0) "

getHPBar :: Integer -> Integer -> [Char]
getHPBar currHP maxHP
  | currHP < 1 = (getHPBarHelp 20 0)
  | otherwise = (getHPBarHelp 20 (round ((fromIntegral currHP) / (fromIntegral maxHP) * 20)))

getHPBarHelp :: Integer -> Integer -> [Char]
getHPBarHelp numberleft fulls
  | numberleft == 0 = []
  | fulls == 0 = '-':(getHPBarHelp (numberleft - 1) 0)
  | otherwise = "/.b#/.0"++(getHPBarHelp (numberleft - 1) (fulls - 1))

getHPBarOpp :: Integer -> Integer -> [Char]
getHPBarOpp currHP maxHP
  | currHP < 1 =(getHPBarHelpOpp 20 0)
  | otherwise = (getHPBarHelpOpp 20 (round ((fromIntegral currHP) / (fromIntegral maxHP) * 20)))

getHPBarHelpOpp :: Integer -> Integer -> [Char]
getHPBarHelpOpp numberleft fulls
    | numberleft == 0 = []
    | fulls == 0 = '-':(getHPBarHelpOpp (numberleft - 1) 0)
    | otherwise = "/.r#/.0"++(getHPBarHelpOpp (numberleft - 1) (fulls - 1))


testGetHPBar :: IO()
testGetHPBar = do
  putStrLn $ colourize $ (getHPBar 15 20)

thisManySpaces :: Int -> [Char]
thisManySpaces n
  | n == 0 = []
  | otherwise = ' ':(thisManySpaces (n - 1))



{-getAIFromTree attacker enemy =
    decide D  { dName = "", attributes = [(ap,show (ap attacker)),
              (aiclass,show (playerClass attacker)),
              (playerclass,show (playerClass enemy)),
              (recommendedatk,"0")]}
-}
