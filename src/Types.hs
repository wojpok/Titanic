{-# LANGUAGE ExistentialQuantification
           , StandaloneDeriving 
           , FunctionalDependencies
           , MultiParamTypeClasses
           #-}

module Types where

import Depth

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

data Line
  = LString String
  | LChar Char
  | LConcat Line Line
  | LAlignLeft Line
  | LFill String Int
  | LEmpty
  | LColor Color
  | LColorPop 
  deriving (Show, Eq, Ord)

data Doc = Doc DocTree Width
  deriving Show

class DocConfCustom a c | a -> c where
  confDocLines :: c -> Int -> Int -> a -> CtxBox

ppConfCustom :: DocConfCustom a c => c -> a -> Doc
ppConfCustom conf x = Doc (DCustom x (confDocLines conf)) $ fixedWidth 0

class DocCustom a where
  docLines :: Int -> Int -> a -> CtxBox

ppCustom :: DocCustom a => a -> Doc
ppCustom x = Doc (DCustom x docLines) $ fixedWidth 0

data DocTree 
  = DEmpty
  | DString    String
  | DAlignS    Doc
  | DAlignR    Doc
  | DSeq      [Doc]
  | DStack    [Doc]
  | DEither    Doc   Doc
  | DLayout    Int   Doc
  | DColor     Color Doc
  | DBox       Doc
  | DFlex      Int   Doc
  | forall a. DCustom a (Int -> Int -> a -> CtxBox)   

instance Show DocTree where
  show DEmpty        = "(Empty)"
  show (DColor c s)  = "(Color "  ++ show s ++ ")"
  show (DBox s)      = "(Box "    ++ show s ++ ")"
  show (DString s)   = "(String " ++ show s ++ ")"
  show (DAlignS d)   = "(Shift "  ++ show d ++ ")"
  show (DAlignR d)   = "(Reset "  ++ show d ++ ")"
  show (DSeq seq)    = "(Seq "    ++ foldr (\r l -> show r ++ " " ++ l) "" seq ++ ")"
  show (DStack seq)  = "(Stack "  ++ foldr (\r l -> show r ++ " " ++ l) "" seq ++ ")"
  show (DEither l r) = "(Either " ++ show l ++ " " ++ show r ++ ")"
  show (DLayout n d) = "(Lay "    ++ show n ++ " " ++ show d ++ ")"
  show (DFlex e d)   = "(DFlex "  ++ show e ++ " " ++ show d ++ ")"
  show (DCustom _ _) = "(Custom #)"

type CtxBox = ([Line], Int)
