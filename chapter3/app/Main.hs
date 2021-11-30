{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

import Data.Time

data Params
data QuoteDataCollection
data StatInfo
data Html

work :: Params -> IO ()
work = _

readQuotes :: FilePath -> IO QuoteDataCollection
readQuotes = _

statInfo :: QuoteDataCollection -> StatInfo
statInfo = _

textReport :: StatInfo -> String
textReport = _

plotChart :: QuoteDataCollection -> IO ()
plotChart = _

htmlReport :: QuoteDataCollection -> StatInfo -> Html
htmlReport = _

saveHtml :: FilePath -> Html -> IO ()
saveHtml = _

main :: IO ()
main = putStrLn "stock quotes project"
