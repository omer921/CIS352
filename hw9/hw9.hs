-- Omer Winrauke

import System.Random
import Data.List

getInt :: IO Int
getInt = do { item <- getLine ; return (read item :: Int) }


showHisto :: IO()
showHisto = do n <- getInt
               if (n<0) then printAll n else showHisto
               new <- printAll n
               return (new)
               

printAll :: Int -> IO()
printAll n = do val <- putStrLn(replicate n '*')
                return (val)
               

ask :: String -> IO Char
ask str = do{
          putStrLn (str);
           val <- getLine;
          return (val !! 0)
          }

game :: IO ()
game = do
       putStrLn "Choose a number between 1 and 100"
       let bottom = 1
       let top = 100
       theGame bottom top

theGame :: Int -> Int -> IO ()
theGame low upp = do
                   num <- randomRIO (low,upp) :: IO Int
                   putStrLn ("Is the number "++show num++"?")
                   stream <- getLine
                   if stream=="yes"
                      then putStrLn "I win!"
                      else do
                           let nLow = if stream=="higher" then num else low
                           let nUpp = if stream=="lower" then num else upp
                           theGame nLow nUpp
