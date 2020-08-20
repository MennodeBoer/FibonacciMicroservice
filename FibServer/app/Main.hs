module Main where

import Lib
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently)


main :: IO ()
main = const () <$> concurrently (runFibApp One) (runFibApp Two)
