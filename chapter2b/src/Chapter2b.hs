{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Chapter2b where

import Data.Function -- imports (&) operator for pipelining
import Fmt
import qualified Data.Text as DText

doChapter2b :: String
doChapter2b = "Chapter2b"

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
  t1 <> t2 = t2 <> t1 -- specifies commutativity

instance Monoid Turn where
  mempty = TNone

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

-- requires StandaloneDeriving
deriving instance Read Direction
deriving instance Read Turn

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

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
  f <- readFile fname
  let turns = lines f
            & map read
      finalDir = rotateMany' dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +||finalDir||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

--orientFromFile :: FilePath -> IO ()


