{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses 
           , Rank2Types
           #-}

module Colors where

import qualified System.Console.ANSI as A
import qualified Control.Concurrent as T
import Control.Monad

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

ansiTest :: IO ()
ansiTest = do
  A.useAlternateScreenBuffer
  Just (x, y) <- A.getTerminalSize
  A.setSGR [colorCommandBg $ A.Cyan, colorCommandFg A.Red]
  forM_ [1..x * y] $ const $ do putStr "x"

  A.setSGR [colorCommandBg $ A.Green, colorCommandFg A.Red]
  A.setCursorPosition 0 0
  forM_ [1..4] $ do \x -> do putStrLn ("Hello world " ++ show @Int x)
                             T.threadDelay 500000
  A.setSGR [colorCommandBg $ A.Green, colorCommandFg A.Yellow]
  forM_ [1..4] $ do \x -> do putStrLn ("Hello world " ++ show @Int x)
                             T.threadDelay 500000
  --Control.Concurrent.threadDelay 5000000
  A.useNormalScreenBuffer

