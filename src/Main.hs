-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main(
    main
) where

import System.Environment
import DT
import Data.List

main :: IO ()
main = do
  args <- getArgs
  database <- readDatabase $ head args
  let attrs = attributes database
  let rws = rows database
  let clsDom = classDomain database
  let trSize = read (head $ tail args) :: Int
  let training = Db attrs (take trSize rws) clsDom
  let testRows = drop trSize rws
  let dt = build_dt training
  print dt

  --foldr (\(row, trueLabel) -> if ((classify attrs dt row) == trueLabel) then 1 else 0) 0 testRows

  mapM_ putStrLn $ map (\(row,trueLabel) -> intercalate ", " row ++
                                         " classified as " ++
                                         classify attrs dt row ++
                                         ", true label is " ++ trueLabel ) testRows
  putStrLn ""
  mapM_ putStrLn $ toRules dt

