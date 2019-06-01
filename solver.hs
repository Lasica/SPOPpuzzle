module Main where

import System.Console.ANSI as ANSI
import System.Environment ( getArgs )
import Data.Matrix

type Field = Maybe Color
type Row = [Field]
type Board = Matrix Field

data BlockInfo = BlockInfo{
  color::Color,
  count::Int
  }deriving (Show,Read) 

type LineInfo = [BlockInfo]

data Puzzle = Puzzle {
  width::Int,
  height::Int,
  leftInfo::[LineInfo],
  upperInfo::[LineInfo]
  }deriving (Show,Read)   

-- co to robi?
getFieldSGR :: Color -> [SGR]
getFieldSGR field = [ SetColor Background Vivid field  ]

drawField :: Color -> IO ()
drawField field = do
  setSGR $ getFieldSGR field
  putChar ' '

-- solve :: Puzzle -> Board
solve puzzle = solution
    where
        solution = 0
    -- obviousHorizontal = map (fillObviousHorizontal (width puzzle)) (leftInfo puzzle)
    -- obviousHorizontalMatrix = fromLists obviousHorizontal :: Matrix Field
    -- obvious = fillObviousVertical puzzle obviousHorizontalMatrix
    -- solution = obvious

numericalTruth True = 1
numericalTruth False = 0

necessaryGaps (a:b:linfo) = numericalTruth ((color a) == (color b)) + (necessaryGaps (b:linfo))
necessaryGaps (c:cc) = 0
necessaryGaps [] = 0

lineTotalLength (x:xs) = count x + lineTotalLength xs
lineTotalLength [] = 0

numberOfCombinations rowInfo rowSize = binom (gapsInRow + segments) (segments)
  where
    gapsInRow = rowSize - (lineTotalLength rowInfo) - (necessaryGaps rowInfo)
    binom n k = product [1+n-k..n] `div` product [1..k]
    segments = length rowInfo
    

nextLinePermutation [] bins gaps = gaps : replicate (bins-1) 0
nextLinePermutation perm bins gaps = reverse (carryOne (reverse perm) 0) 
  where
    carryOne (f:s:xs) sumThusFar | s == 0    = case (carryOne (s:xs) (sumThusFar+f)) of
                                                  [] -> []
                                                  carriedOne -> 0:carriedOne
                                 | otherwise = (sumThusFar+f+1):(s-1):xs
    carryOne ls _ = []

printAllPermutations perm bins gaps = do
  putStrLn $ show perm
  case nextLinePermutation perm bins gaps of
    [] -> putStrLn "Done"
    np -> do 
            printAllPermutations np bins gaps
--firstPermutation row_
--nextPermutation permutation row_width row_spec = 

main = do
  -- drawField 0
  -- drawField 1
  -- setSGR [Reset]  -- Reset to default colour scheme
  -- putStrLn ""
  test 0 (necessaryGaps [])
  test 1 (necessaryGaps [BlockInfo {color=Red, count=2}, BlockInfo{color=Red, count=3}])
  test 0 (necessaryGaps [BlockInfo {color=Red, count=2}, BlockInfo{color=Yellow, count=3}])
  test 10 (numberOfCombinations [BlockInfo {color=Red, count=2}, BlockInfo {color=Red, count=2}] 8)
  test [3,0,0] (nextLinePermutation [] 3 3)
  test [2,0,1] (nextLinePermutation [2,1,0] 3 3)
  test [0,3,0] (nextLinePermutation [1,0,2] 3 3)
  test [] (nextLinePermutation [0,0,3] 3 3)
  putStrLn "tests passed"

  args <- getArgs
  line <- readFile (head args)
  let puzzle = read line :: Puzzle 
  putStrLn $ show $ solve puzzle

test expect expr = 
  if expect == expr
  then putChar '.'
  else putStrLn $ (show expect ++" != "++ show expr)
  
-- generator wierszy
-- sprawdzacz wierszy
-- sprawdzacz planszy
-- wydruk planszy 