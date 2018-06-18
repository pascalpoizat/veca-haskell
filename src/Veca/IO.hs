{-|
Module      : Veca.IO
Description : IO functions for Veca
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Veca.IO (genXTA
               ,writeToXTA
               ,genJSON
               ,readFromJSON
               ,writeToJSON)
where

import           Data.Aeson            (decode, encode)
import qualified Data.ByteString.Lazy  as BS
import           Models.TimedAutomaton
import           Veca.Model
import           Veca.Operations       (cToCTree, cTreeToTAList)

{-|
Generate the XTA representation for a component instance.
-}
genXTA :: ComponentInstance -> String
genXTA = m2t . m2m
  where
    m2t = asXta
    m2m = TimedAutomataNetwork . cTreeToTAList . cToCTree

{-|
Write a component instance to a XTA file.
-}
writeToXTA :: FilePath -> ComponentInstance -> IO ()
writeToXTA p = writeFile p . genXTA

{-|
Generate the JSON representation for a component instance.
-}
genJSON :: ComponentInstance -> BS.ByteString
genJSON = encode

{-|
Read a component instance from a JSON file.
-}
readFromJSON :: FilePath -> IO (Maybe ComponentInstance)
readFromJSON p = decode <$> BS.readFile p

{-|
Write a component instance to a JSON file.
-}
writeToJSON :: FilePath -> ComponentInstance -> IO ()
writeToJSON p = BS.writeFile p . encode
