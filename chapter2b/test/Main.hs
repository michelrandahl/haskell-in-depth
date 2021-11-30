{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Chapter2b

prop_test :: Property
prop_test = property $ do
  doChapter2b === "Chapter2b"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
