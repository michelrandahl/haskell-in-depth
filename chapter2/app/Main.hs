{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&)) -- imports (&) operator for pipelining
import Fmt
import System.Environment
import System.Random.Stateful
import Control.Monad (replicateM, unless)
import Data.List (nub, sort)
import System.Exit (exitFailure)
--import Data.Semigroup
--import qualified Data.Text
import Data.Fixed
import Control.Monad.Writer

class Stuff a where
  foo :: a
  bar :: a

instance Stuff Int where
  foo = 42
  bar = 50

foobar :: Int
foobar = foo

myEvery :: (Stuff a, Integral a) => [a]
myEvery = [foo..bar]

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True  

yes :: Bool
yes = yesno (42 :: Int)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, Show, CyclicEnum)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Enum, Bounded, Show)

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orient :: Direction -> Direction -> Turn
orient d1 d2 = every
             & filter (\t -> rotate t d1 == d2)
             & head

orientMany :: [Direction] -> [Turn]
orientMany directions@(_:_:_) = zipWith orient directions (tail directions)
orientMany _ = []

instance Semigroup Turn where
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty = TNone

deriving instance Read Direction
deriving instance Read Turn
deriving instance Eq Turn
deriving instance Ord Turn

instance UniformRange Direction where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Direction where
  uniformM rng = uniformRM (minBound, maxBound) rng

instance UniformRange Turn where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Turn where
  uniformM rng = uniformRM (minBound, maxBound) rng

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

myval1 :: IO Direction
myval1 = uniformIO

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

writeRandomFile :: (Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen file_name = do
  xs <- gen n
  writeFile file_name $ unlines $ map show xs

test_allTurnsInUse :: Bool
test_allTurnsInUse =
  sort (nub [ orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts =
  and [rotateMany d ts == rotateMany' d ts | d <- every]

test_orientRotateAgreee :: [Direction] -> Bool
test_orientRotateAgreee [] = True
test_orientRotateAgreee ds@(d:_) = ds == rotateManySteps d (orientMany ds)

test_main :: IO ()
test_main = do
  ds <- randomDirections 1000
  ts <- randomTurns 1000
  unless (and [ test_allTurnsInUse
              , test_orientRotateAgreee ds
              , test_rotationsMonoidAgree ts ])
    exitFailure

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir file_name = do
  f <- readFile file_name
  let turns = map read $ lines f
      finalDir = rotateMany' dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile file_name = do
  f <- readFile file_name
  let directions = map read $ lines f
      orientations = orientMany directions
  fmt $ nameF "Orientations" (unwordsF orientations)
      

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", file_name, direction] -> rotateFromFile (read direction) file_name
    ["-o", file_name] -> orientFromFile file_name
    _ -> putStrLn $ "Usage: locator -o filename\n" ++
                    "       locator -r filename direction"

{-
  $ cabal -v0 run chapter2 -- -r turns.txt North
  Final direction: South
  Intermediate directions: N W S S


  $ cabal -v0 run chapter2 -- -o dirs.txt
  Orientations: <- ||
-}

{-
   start repl with
   $ cabal repl
   > :load app/Main.hs
-}

{-
  to use functions such as nameF in repl, first enable
  $ :set -XOverloadedStrings
-}

data E4
instance HasResolution E4 where
  resolution _ = 10000
type Fixed4 = Fixed E4

sumN :: Int -> Writer String Int
sumN 0 = writer (0, "finish")
sumN n = do
  tell (show n ++ ",")
  s <- sumN (n-1)
  pure (n + s)

cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure (x,y)


