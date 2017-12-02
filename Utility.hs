module Utility where

import Data.Maybe
import Text.Read
  --Colourize: adds code to text to colourize when it is printed
  -- I wrote this with color info taken from  http://www.markhneedham.com/blog/2012/04/29/haskell-colour-highlighting-when-writing-to-the-shell/


colourize :: [Char] -> [Char]
colourize x = do
    let
      cchange "/.g" = "\x1b[32m"
      cchange "/.b" = "\x1b[34m"
      cchange "/.c" = "\x1b[34m"
      cchange "/.r" = "\x1b[31m"
      cchange "/.y" = "\x1b[33m"
      cchange "/.0" = "\x1b[30m"
      cchange "/.m" = "\x1b[35m"
      cchange "/.w" = "\x1b[37m"
      c = c
    if x == []
      then "\x1b[30m"
      else if (tail x) == []
        then (head x):"\x1b[30m"
        else if (head x) == '/' && (head (tail x)) == '.'
          then (cchange ((head x):(head (tail x)):(head (tail (tail x))):[]))  ++ (colourize (tail (tail (tail x))))
          else (head x):(colourize (tail x))


humanInt ::  Integer -> IO Integer
humanInt max = do
  res <- getLine
  let maybeInt = readMaybe res
--  let m = case maybeInt of
--    Nothing -> 0
  --  Just x -> x
  let m = fromMaybe 4000 maybeInt
  {--if temp == Nothing then do
    putStrLn $ "Invalid input, please try again!"
    humanInt max
    else do
      m <---}
  if m > max then do
    putStrLn $ "Invalid input, please try again!"
    humanInt max
    else do return(m)




testFunc bool = do
  let peep = if bool then "sick" else "doom"
  return(peep)

getIOBool :: Bool -> IO Bool
getIOBool b = do return(b)
