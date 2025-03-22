{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses 
           , Rank2Types
           #-}

module Colors where

import qualified System.Console.ANSI as A

import Types

translateColor :: Color -> A.Color 
translateColor c = case c of
  CBlack   -> A.Black
  CRed     -> A.Red
  CGreen   -> A.Green
  CYellow  -> A.Yellow
  CBlue    -> A.Blue
  CMagenta -> A.Magenta
  CCyan    -> A.Cyan
  CWhite   -> A.White

readColor :: String -> Color
readColor "red" = CRed
readColor "green" = CGreen
readColor "yellow" = CYellow
readColor "blue" = CBlue
readColor _ = CWhite

colorCommandBg :: A.Color -> A.SGR 
colorCommandBg col = A.SetColor A.Background A.Vivid col

colorCommandFg :: A.Color -> A.SGR 
colorCommandFg col = A.SetColor A.Foreground A.Vivid col

ansiColorFg :: Color -> String
ansiColorFg c = A.setSGRCode [colorCommandFg $ translateColor c]

ansiColor :: Color -> Color -> String
ansiColor fg bg =
  let commFg = colorCommandFg $ translateColor fg  
      commBg = colorCommandBg $ translateColor bg
  in A.setSGRCode [commFg, commBg] 

ansiColorReset :: String
ansiColorReset = A.setSGRCode [A.Reset] 

testColor :: IO ()
testColor = do
  putStr $ ansiColor CRed CGreen 
  putStr $ "Hello World"
  putStrLn ansiColorReset
