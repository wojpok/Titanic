{-# LANGUAGE AllowAmbiguousTypes
           , TypeApplications
           , TypeSynonymInstances
           , FlexibleInstances
           , MultilineStrings
           , Rank2Types
           , ConstrainedClassMethods
           , InstanceSigs
           , ScopedTypeVariables
           #-}

module Pretty3 where

import Parser 
import Types
import Numeric (showHex)
import Pretty2
import Depth
import Colors
import Borders

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Either

type Err a = Either String a

raise :: String -> Err a
raise = Left 

type Formatter a = CoreStyle -> Err (a -> Doc)

lookupE :: Eq a => a -> [(a, b)] -> Err b
lookupE _ [] = Left ""
lookupE x ((y, v):ys)
  | x == y = return v
  | otherwise = lookupE x ys 

getSStr :: String -> CoreStyle -> Err String
getSStr ident (CoreStyle _ sty _) = do
  v <- lookupE ident (getStyleDict sty)
  case v of
    SString s -> return s
    _ -> Left "Expected string" 

getSStrI :: String -> CoreStyle -> Err String
getSStrI ident n = getSStr "im" n `alter` getSStr ident n

getSInt :: String -> CoreStyle -> Err Int
getSInt ident (CoreStyle _ sty _) = do
  v <- lookupE ident (getStyleDict sty)
  case v of
    SInt s -> return s
    _ -> Left "Expected Int" 

getSIntI :: String -> CoreStyle -> Err Int
getSIntI ident n = getSInt "im" n `alter` getSInt ident n

getSCol :: String -> CoreStyle -> Err Color 
getSCol ident (CoreStyle _ sty _) = do
  v <- lookupE ident (getStyleDict sty)
  case v of
    SColor s -> return s
    _ -> Left "Expected string" 

getSColI :: String -> CoreStyle -> Err Color
getSColI ident n = getSCol "im" n `alter` getSCol ident n

alter :: Err a -> Err a -> Err a
alter (Right x) _ = Right x
alter _ r = r

assertSingle :: CoreStyle -> Err CoreStyle
assertSingle (CoreStyle _ _ [CoreInterp _ x]) = Right x
assertSingle _ = Left "Arrity mismatch"

fmtLift :: forall a. Formatter a -> Formatter a
fmtLift cont c@(CoreStyle name sty struct) =
  case name of
    "box" -> do tl <- assertSingle c
                fmt <- fmtLift cont tl
                return (ppBox . fmt)
    "color" -> do tl <- assertSingle c
                  col <- getSColI "col" c
                  fmt <- fmtLift cont tl
                  return (ppColor col . fmt)
    "shift" -> do tl <- assertSingle c
                  (ppAlignS .) <$> fmtLift cont tl
    "reset" -> do tl <- assertSingle c
                  (ppAlignR .) <$> fmtLift cont tl
    "string" -> do text <- getSStrI "str" c
                   return $ const $ ppString text
    "line" -> return $ const $ ppHLine
    "flex" -> do flex <- getSIntI "w" c
                 return (const $ ppFlex flex)
    "lay" -> do tl <- assertSingle c
                flex <- getSIntI "w" c
                fmt <- fmtLift cont tl
                return (ppLayout flex . fmt)
      
    _ -> cont c

class Formatable a where
  name :: a -> String
  fmt :: Formatter a

instance Formatable () where
  name = const "unit"
  fmt = fmtLift $ \_ -> return $ const ppEmpty

instance Formatable Int where
  name = const "int"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "repr" doc `alter` pure "n"
    case t of
      "n" -> return (ppShow)
      "x" -> return (ppString . ("0x" ++) . flip showHex "")
      _ -> raise "Unknown representation"

instance Formatable Bool where
  name = const "bool"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "repr" doc `alter` pure "n"
    case t of
      "n" -> return ppShow
      "lisp" -> return $ \x -> ppShow $ if x then "#t"   else "#f"
      "ml"   -> return $ \x -> ppShow $ if x then "true" else "false"
      "num"  -> return $ \x -> ppShow $ if x then "1"    else "0"

instance Formatable a => Formatable [a] where
  name = const "list"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "orient" doc `alter` pure "ver"
    r <- assertSingle doc
    fmtRec <- fmt r
    case t of
      "hor" -> do spacing <- getSStr "s" doc `alter` pure ""
                  return (ppSeq . interleave spacing . map fmtRec) where
                    interleave spacing [] = []
                    interleave spacing [x] = [x]
                    interleave spacing (x:xs) = (x : ppString spacing : interleave spacing xs)
      "ver" -> return (ppStack . map fmtRec)

instance (Formatable a, Formatable b) => Formatable (a, b) where
  name = const "tuple2"
  fmt = fmtLift $ \doc -> do
    let (CoreStyle _ _ struct) = doc
        mapped = map recf struct
        recf :: CoreInterp -> Err ((a, b) -> Doc) 
        recf (CoreInterp 1 f) = fmt @a  f >>= \fx -> pure (fx . fst)
        recf (CoreInterp 2 f) = fmt @b  f >>= \fx -> pure (fx . snd)
        recf (CoreInterp _ f) = fmt @() f >>= \fx -> pure (fx . const ())
    orient <- getSStrI "orient" doc `alter` pure "hor" 
    fs <- sequence mapped
    case orient of
      "hor" -> return $ \p -> (ppSeq $ fs <*> [p])
      "ver" -> return $ \p -> (ppStack $ fs <*> [p])

pipe :: Formatable a => String -> a -> Err Doc
pipe style d = do
  sty <- l $ parseStyle style 
  f <- fmt $ desugar sty
  return $ f d
    where
      l (Left _) = Left "Parse error"
      l (Right x) = Right x

makeFmt :: Formatable a => String -> a -> IO ()
makeFmt style d = do
  case pipe style d of
    Left e -> putStrLn e
    Right d -> do inspectDoc d
                  putStr $ showDoc d

testNewFormat = 
  makeFmt @[[Int]]
          """
          (color:red
            *(list:hor
              (list {s: ' ', orient: ver} 
                &(box 
                  (color:cyan 
                    (int {repr: 'x'})
                  )
                )
              )
            )
          )
          """
          [[10, 20, 30], [5000, 50000, 6000]]

testNewFormat2 = 
  makeFmt @((Int, Int), (Int, Int))
          """
          (lay:40
          *(color:red
            (box
              (color:white
                (tuple2:ver
                  1(tuple2:hor
                    (flex:5)
                    1(int)
                    (flex:5)
                    ' '
                    2&(int)
                    (flex:5)
                  )
                  (color:red (line))
                  2(tuple2:hor
                    (flex:10)
                    1(int)
                    ' '
                    2&(int)
                    (flex: 20)
                  )
                )
              )
            )
          )
          )
          """
          ((1, 22334), (3324324, 4))

pato :: Doc
pato = ppAlignR $ ppSeq [ ppStack [ ppShow "aaa"
                                  , ppSeq [ ppShow "bbbbb"
                                          , ppAlignS $ ppShow "yyyy"
                                          ]
                                  ]
                        , ppAlignS $ ppShow "x"
                        ]
