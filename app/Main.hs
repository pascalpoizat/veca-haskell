{-|
Module      : Main
Description : VECA main program
Copyright   : (c) 2017 Pascal Poizat
License     : Apache-2.0 (see the file LICENSE)
Maintainer  : pascal.poizat@lip6.fr
Stability   : experimental
Portability : unknown
-}
module Main where

import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative

{-|

usage:

veca list
to get the list of all examples

veca dump EXAMPLE PATH --xta
veca dump EXAMPLE PATH --json
dump the given example to json or xta

veca transform PATH
transform a json veca model to xta

-}

version :: String
version = "1.0.1"

newtype Options = Options
  { optCommand :: Command
  }

data Command
  = List
  | Dump { optExample :: String
         , optPath :: String
         , optFormat :: DumpFormat }
  | Transform { optPath :: String }

data DumpFormat
  = XTA 
  | JSON

parserOptions :: Parser Options
parserOptions = Options <$> 
  subparser (
    command "list" (info (pure List) (progDesc "list internal examples"))
    <> command "internal" (info parserDump (progDesc "work with an internal example"))
    <> command "transform" (info parserTransform (progDesc "transform a VECA model to UPPAAL")))

parserDump :: Parser Command
parserDump = Dump <$>
  argument str (metavar "EXAMPLE" <> help "name of the internal example to use")
  <*> argument str (metavar "PATH" <> help "path of the output (without the suffix)")
  <*> parserFormat

parserFormat :: Parser DumpFormat
parserFormat = parserDumpToXTA <|> parserDumpToJSON

parserDumpToXTA :: Parser DumpFormat
parserDumpToXTA = flag' XTA ( long "xta" <> help "transform an internal VECA model example into an UPPAAL xta file")

parserDumpToJSON :: Parser DumpFormat
parserDumpToJSON = flag' JSON (long "json" <> help "dump an internal VECA model example in the VECA json format")

parserTransform :: Parser Command
parserTransform =
  Transform <$>
  argument str
    (metavar "PATH" <>
     help
       "path to the input VECA model in JSON format (without the .json suffix)")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parserOptions <**> helper)
        (fullDesc <> progDesc "transformations on VECA models" <>
         header ("veca " <> version))

run :: Options -> IO ()
run (Options List) = putStrLn "rover"
run (Options (Transform p)) = transform p
run (Options (Dump e p f)) =
  case f of
    XTA -> dumpExampleAsXTA p e
    JSON -> dumpExampleAsJSON p e
