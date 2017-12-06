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
import           Veca.Veca

{-|
Generate the XTA representation for a component.
-}
genXTA :: Component -> String

genXTA = m2t . m2m
  where
    m2t = asXta 
    m2m = TimedAutomataNetwork . flatten . cToTATree

{-|
Write a component to a XTA file.
-}
writeToXTA :: FilePath -> Component -> IO ()
writeToXTA p = writeFile p . genXTA

{-|
Generate the JSON representation for a component.
-}
genJSON :: Component -> BS.ByteString
genJSON = encode

{-|
Read a component from a JSON file.
-}
readFromJSON :: FilePath -> IO (Maybe Component)
readFromJSON p = decode <$> BS.readFile p

{-|
Write a component to a JSON file.
-}
writeToJSON :: FilePath -> Component -> IO ()
writeToJSON p = BS.writeFile p . encode
