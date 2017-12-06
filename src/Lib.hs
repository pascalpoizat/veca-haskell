{-|
Module      : Lib
Description : VECA library (sample functions, may soon be removed)
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Lib
where

import           Examples.Rover.Model
import           Veca.IO

{-|
Sample function to generate the XTA output for the Rover example.
-}
dumpRoverAsXTA :: IO ()
dumpRoverAsXTA = writeToXTA "/tmp/rover.xta" rover

{-|
Sample function to dump the Rover example into JSON.
-}
dumpRoverAsJSON :: IO ()
dumpRoverAsJSON = writeToJSON "/tmp/rover.json" rover

{-|
Sample function to read a component in JSON format and dump it in XTA format.
-}
workOnRover :: IO ()
workOnRover = do
    mc <- readFromJSON "/tmp/rover.json"
    case mc of
        Nothing -> putStrLn "error"
        Just c -> do
            writeToXTA "/tmp/rover.xta" c
            putStrLn "ok"
