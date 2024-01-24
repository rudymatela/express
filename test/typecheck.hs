-- test/typecheck.hs: imports Express and does nothing
--
-- to be used for triggering typechecking using runhugs
--
-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Data.Express.Core
import Data.Express.Hole
import Data.Express.Map
import Data.Express.Match

import Data.Express.Name
import Data.Express.Triexpr

import Data.Express.Utils
import Data.Express.Utils.Typeable

main :: IO ()
main  =  return ()
