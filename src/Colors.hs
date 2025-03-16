{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses 
           #-}

module Colors where

import qualified System.Console.ANSI as A

import Types
import Pretty
import Pretty2

data Color
  = CBlack
  | CRed
  | CGreen
  | CYellow
  | CBlue
  | CMagenta
  | CCyan
  | CWhite
  deriving (Show, Eq, Ord)

data ColorDoc = ColorDoc { docColor :: Maybe (Color, Color)
                         , docRest  :: Doc 
                         }
    deriving (Show)

instance DocConfCustom ColorDoc (Maybe (Color, Color)) where
  confDocLines col size d node = 
    let (lines, s) = toLines size d (docRest node)
    in case (docColor node) of
      Nothing -> (map (LConcat (LString ansiColorReset)) lines, s)
      Just (fg, bg) ->
        let color = ansiColor fg bg
        in (map (\e -> (LConcat (LString color) (LConcat e (LString ansiColorReset)))) lines, s)

{-
ppColor :: Color -> Color -> Doc -> Doc
ppColor fg bg d@(doc, wa, b, wb) = 
  (DocConfCustom (Just (fg, bg)) d, wa, b, wb)

ppColorReset :: Doc -> Doc
ppColorReset d@(doc, wa, b, wb) =
  (DocConfCustom Nothing d, wa, b, wb)
-}

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


