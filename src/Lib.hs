{-|
Module      : Lib
Description : VECA library
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Lib
where

import           Data.Monoid          ((<>))
import           Examples.Rover.Model
import           Veca.IO

{-|
Sample function to generate the XTA output for an example.
-}
dumpExampleAsXTA :: String -> String -> IO ()
dumpExampleAsXTA path example = case example of
  "rover" -> do
    writeToXTA (path <> ".xta") rover
    putStrLn "generation done"
  _ -> putStrLn "unknown example"

{-|
Sample function to dump an example into JSON.
-}
dumpExampleAsJSON :: String -> String -> IO ()
dumpExampleAsJSON path example = case example of
  "rover" -> do
    writeToJSON (path <> ".json") rover
    putStrLn "generation done"
  _ -> putStrLn "unknown example"

{-|
Sample function to read a component in JSON (to check if format is ok).
-}
read :: String -> IO ()
read path =
  let input  = path <> ".json"
      output = path <> ".xta"
  in  do
        mc <- readFromJSON input
        case mc of
          Nothing         -> putStrLn $ "error could not read file " <> input
          Just aComponent -> putStrLn "reading done"

{-|
Sample function to read a component in JSON format and dump it in XTA format.
-}
transform :: String -> IO ()
transform path =
  let input  = path <> ".json"
      output = path <> ".xta"
  in  do
        mc <- readFromJSON input
        case mc of
          Nothing         -> putStrLn $ "error could not read file " <> input
          Just aComponent -> do
            writeToXTA output aComponent
            putStrLn "generation done"
