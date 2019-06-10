module Main where

import System.Console.ANSI as ANSI
import System.Environment ( getArgs )
import Data.Matrix
import PutBoard

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

solve :: Puzzle -> Board
solve puzzle = solution
  where 
    obviousHorizontal = map (fillObviousHorizontal (width puzzle)) (leftInfo puzzle)
    obviousHorizontalMatrix = fromLists obviousHorizontal :: Matrix Field
    obvious = fillObviousVertical puzzle obviousHorizontalMatrix
    solution = obvious

fillObviousVertical puzzle board = transpose filledVerticaly
  where
    listOfColumns = toLists (transpose board)
    obviousVertical = map (fillObviousHorizontal (height puzzle)) (upperInfo puzzle)
    obviousFlipedLists = zipWith (zipWith f) listOfColumns obviousVertical
    filledVerticaly =  fromLists  obviousFlipedLists :: Matrix Field
    f Nothing Nothing = Nothing
    f (Just a) Nothing = Just a
    f Nothing (Just b) = Just b
    f (Just a) (Just b) = Just a

--isCorrectState :: Puzzle -> Board -> Bool
--isCorrectState puzzle boardM = 
--  (go (leftInfo puzzle) (toLists boardM)) && (go (upperInfo puzzle) (toLists(transpose boardM)))
--    where
--      go [] [] = True
--      go [] a  = False
--      go a  [] = False
--      go (lineInfo:info) (row:board) = (checkRow lineInfo row) && (go info board)

-- meybe we generato from one side and then chcek on other
--checkRow :: LineInfo -> Row -> Bool
--checkRow (l:lineInfo) row = take (count l) row 

sumLineInfoBlocks :: LineInfo -> Int
sumLineInfoBlocks lineInfo = go lineInfo 0
  where 
    go [] n = n
    go (x:y:ys) n = if color x == color y 
      then go (y:ys) (n + count x + 1)
      else go (y:ys) (n + count x )
    go (x:xs) n = go xs (n + count x)

fillObviousHorizontal :: Int -> LineInfo -> Row
fillObviousHorizontal lineLength lineInfo 
  | isSimpleFill lineInfo lineLength = simpleFill lineInfo
  | isPartialFill lineInfo lineLength = partialFill lineInfo lineLength
  | otherwise = replicate lineLength Nothing

fieldCodeFromBlockInfo:: BlockInfo -> Color
fieldCodeFromBlockInfo blockInfo = color blockInfo

blockFromInfo :: BlockInfo -> [Field]
blockFromInfo blockInfo =  replicate (count blockInfo) (Just (fieldCodeFromBlockInfo blockInfo))

isSimpleFill :: LineInfo -> Int -> Bool
isSimpleFill lineInfo lineLength = sumLineInfoBlocks lineInfo == lineLength
simpleFill :: LineInfo -> Row
simpleFill lineInfo =  go lineInfo []
  where
    go [] row = row
    go (x:y:ys) row = if color x == color y 
      then go (y:ys) (row ++ blockFromInfo x ++ [Just  White]) 
      else go (y:ys) (row ++ blockFromInfo x )
    go (x:xs) row = go xs (row ++ (blockFromInfo x) )

isPartialFill :: LineInfo -> Int -> Bool
isPartialFill lineInfo lineLength = 
  length lineInfo == 1 
  && count (head lineInfo)  >   lineLength `div` 2

partialFill :: LineInfo -> Int -> Row
partialFill lineInfo lineLength = 
  replicate uncertainPartLength   Nothing
  ++ replicate certainPartLength (Just certainColor)
  ++ replicate uncertainPartLength  Nothing
  where   
    uncertainPartLength =  lineLength - count (head lineInfo)
    certainColor = fieldCodeFromBlockInfo (head lineInfo)
    certainPartLength = lineLength - uncertainPartLength - uncertainPartLength

main = do
  -- drawField 0
  -- drawField 1
  -- setSGR [Reset]  -- Reset to default colour scheme
  -- putStrLn ""
  test  0 ( sumLineInfoBlocks [] )
  test  1 ( sumLineInfoBlocks  [BlockInfo {color=Red, count=1}] )
  test  3 ( sumLineInfoBlocks  [BlockInfo {color=Red, count=1}, BlockInfo {color=Red, count=1}] )
  test  2 ( sumLineInfoBlocks  [BlockInfo {color=Green, count=1}, BlockInfo {color=Red, count=1}] )
  test  5 ( sumLineInfoBlocks  [BlockInfo {color=Green, count=2}, BlockInfo {color=Red, count=1},
    BlockInfo {color=Red, count=1}] )

  test  [Nothing,Nothing] ( fillObviousHorizontal 2 [BlockInfo {color=Red, count=1}]  )
  test  [Just Red] ( fillObviousHorizontal 1 [BlockInfo {color=Red, count=1}] )
  test  [Just Yellow,Just Green, Just Red]
    ( fillObviousHorizontal 3 [BlockInfo {color=Yellow, count=1}, BlockInfo {color=Green, count=1},
    BlockInfo {color=Red, count=1}] )
  test  [Nothing, Just Green, Nothing]
    ( fillObviousHorizontal 3 [BlockInfo {color=Green, count=2}] )

  test  [Just Green,Just Green, Just Red, Just  White, Just Red ]
    ( simpleFill  [BlockInfo {color=Green, count=2}, BlockInfo {color=Red, count=1},
    BlockInfo {color=Red, count=1}])
  
  test  [Nothing, Just Green, Nothing]
    ( partialFill  [BlockInfo {color=Green, count=2}] 3)
  test  [Nothing,Nothing, Just Green,Just Green,Just Green, Just Green,Just Green,Just Green, Nothing,Nothing]
    ( partialFill  [BlockInfo {color=Green, count=8}] 10)

  test [[Just Black, Nothing], [Just Black, Nothing]] (toLists (fillObviousVertical 
    (Puzzle {width=2, height=2, 
      leftInfo = [[BlockInfo {color = Black, count = 1}],[BlockInfo {color = Black, count = 1}]],
      upperInfo = [[BlockInfo {color = Black, count = 2}],[]]}) 
    (fromLists [[Nothing,Nothing],[Nothing,Nothing]]:: Matrix Field)))

  putStrLn "tests passed"

  args <- getArgs
  line <- readFile (head args)
  let puzzle = read line :: Puzzle 
  putStrLn $ show $ solve puzzle
  putBoard [[]]

test expect expr = 
  if expect == expr
  then putChar '.'
  else putStrLn $ (show expect ++" != "++ show expr)
  
-- generator wierszy
-- sprawdzacz wierszy
-- sprawdzacz planszy
-- wydruk planszy 