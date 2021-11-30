{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Chapter2

prop_test :: Property
prop_test = property $ do
  doChapter2 === "Chapter2"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
