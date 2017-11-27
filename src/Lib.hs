-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  (c) 2017 Pascal Poizat
-- License     :  Apache-2.0 (see the file LICENSE)
--
-- Maintainer  :  pascal.poizat@lip6.fr
-- Stability   :  experimental
-- Portability :  unknown
--
-- A sample module. May soon be removed.
-----------------------------------------------------------------------------

module Lib
    ( someFunc
    ) where

import           Examples.Rover.Model
import           Models.TimedAutomaton
import           Veca.Veca

-- |A sample function printing out a sample string.
someFunc :: IO ()
someFunc = putStrLn $ (asXta . TimedAutomataNetwork . flatten . cToTATree) rover
