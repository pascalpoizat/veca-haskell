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
genXTA = asXta . TimedAutomataNetwork . flatten . cToTATree

{-|
Write a component to a XTA file.
-}
writeToXTA :: FilePath -> Component -> IO ()
writeToXTA p c = writeFile p $ genXTA c

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
writeToJSON p c = BS.writeFile p $ encode c
