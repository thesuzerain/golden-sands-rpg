module Shop where

import Player
import Utility
import Battle
import System.Console.ANSI
import System.Random
import Data.Time.Clock


startShop player = shop player 0

shop :: PlayerData -> Integer -> IO (PlayerData,IO Bool)
shop player boughtSuccessfullyPrevious = do
  clearScreen
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn $ colourize $ "Welcome to the /.bShop/.0! You currently have /.y"++show(currency player)++"/.0G to spend!"
  putStrLn $ colourize $  "Here, you can buy any of the following items or enter (/.b0/.0) to exit:"
  putStrLn $ colourize $  "/.rHP Potion/.0 - /.y50/.0G - (/.b1/.0)"
  putStrLn $ colourize $  "/.rAP Potion/.0 - /.y50/.0G - (/.b2/.0)"
  putStrLn ""
  let purchaseVal = if boughtSuccessfullyPrevious == 0 then "" else if boughtSuccessfullyPrevious == 1 then "Cannot buy!" else "Bought succcesfully!"
  putStrLn purchaseVal
  putStrLn ""
  putStrLn ""
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "======================================="

  response <- humanInt 2
  (playerNew,termiTemp,boughtSuccessfully)<-  case response of
                                  0 -> do return(player,getIOBool True,0)
                                  1 -> tryPurchase HPotion 50 player
                                  2 -> tryPurchase APotion 40 player
  terminateLoop <- termiTemp

  if terminateLoop == True then return(playerNew,getIOBool False) else shop playerNew boughtSuccessfully


tryPurchase :: ItemName -> Integer -> PlayerData -> IO (PlayerData,IO Bool,Integer)
tryPurchase item cost player
  | (currency player) >= cost = do
    let newItems = addToItems (items player) item
    return ((PlayerData (name player) (playerClass player) (hp player) (maxHP player) (ap player) (maxAP player) (level player) (attack player) (defense player) (intelligence player) (dexterity player) ((currency player) - cost) (experience player) newItems), getIOBool False, 2)
  | otherwise = do return (player,getIOBool False,1)

addToItems :: [(ItemName,Integer)] -> ItemName -> [(ItemName,Integer)]
addToItems itemList newItem
  | itemList == [] = [(newItem,1)]
  | (fst (head itemList)) == newItem = (newItem,1+(snd (head itemList))):(tail itemList)
  | otherwise = (head itemList):(addToItems (tail itemList) newItem)
