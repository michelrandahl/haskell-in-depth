{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Chapter1

prop_test :: Property
prop_test = property $ do
  doChapter1 === "Chapter1"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
