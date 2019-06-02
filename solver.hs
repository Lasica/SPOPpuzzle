module Main where

import System.Console.ANSI as ANSI
import System.Environment ( getArgs )
import Data.Matrix
import Data.Bits
import Data.Word -- Word64
import Data.List -- sort
import Data.Maybe
import Data.Function (on)

type Field = Maybe Color
type Row = [Field]
type Board = Matrix Field

data BlockInfo = BlockInfo{
  color::Color,
  count::Int
  }deriving (Show,Read) 
  
data Segment = Segment{
  c::Color,
  endsAt::Int
  }deriving (Show,Read, Eq)

type LineInfo = [BlockInfo]

data Puzzle = Puzzle {
  width::Int,
  height::Int,
  leftInfo::[LineInfo],
  upperInfo::[LineInfo]
  }deriving (Show,Read)   

getFieldSGR :: Color -> [SGR]
getFieldSGR field = [ SetColor Background Vivid field  ]

drawField :: Color -> IO ()
drawField field = do
  setSGR $ getFieldSGR field
  putChar ' '

-- solve :: Puzzle -> Board?
solve puzzle = solution
    where
-- dla planszy wez wiersze i permutuj je aby uzyskac kolumny
-- policz colorSignature dla kolumn
      initialColorSignature = buildColorSignature (upperInfo puzzle)
-- zaczynajac od zestawu pustych permutacji ( [] dla kazdego wiersza)
      initialCombinations = replicate (width puzzle) []
      
-- spermutuj wiersze wg. porzadku by complexity
      rowsOrder = rowOrderByComplexity (width puzzle) (leftInfo puzzle)
      let mySort = sortBy (flip compare `on` fst)
      reversePerm perm = reverse $ map snd $ mySort $ zip perm [0..]
      flipRows order rows = map snd $ reverse $ mySort (zip order rows)
      reorderRows order rows = flipRows (reversePerm order) rows
      originalOrder order rows = flipRows order rows
      
      lineInfo = reorderRows rowsOrder (leftInfo puzzle)
      -- gwarancja ze originalOrder rowsOrder $ reorderRows rowsOrder [1..]  == [1..]
      -- reorderRows z porzadkiem rowsOrder uporzadkuje wg rosnacej mozliwej liczby ulozen wierszy
      
      --nonConflictingCombination comb row = TODO
      -- 1) buildRowFromGaps
      -- 2) dla kazdej komorki z generowanego wiersza poza kolorem bialym przetestuj czy updateColor na colorSignature nie zwroci Nothing
      -- 3) zwroc Nothing jak sie nie udalo albo Just nowa sygnature kolorow
      
      
      solution = originalOrder rowsOrder $ exploreSolutions initialCombinations lineInfo initialColorSignature where
        --exploreSolutions (c:rcomb) (l:linfo) colSig = TODO
        exploreSolutions [] [] _ = Just []
        -- dopoki zaczynajac od wejsciowej kombinacji C wychodzi nonConflictingCombination != Nothing
        -- wywolaj exploreSolutions dla kolejnego wiersza i kombinacji startowej i zwroc liste polaczenia obecnej kombinacji polaczonej z rekursywnym wywolaniem exploreSolutions, chyba ze nastepne exploreSolutions zwroci nothing - wowczas przyjmij wartosc nothing
      
      
      --nextFittingCombination cur_comb (r:rowlist)
-- dla kazdego wiersza
--  wez nastepna permutacje
--    jesli to nie ostatnia permutacja [] to zbuduj wiersz
--    sprawdz czy wiersz pasuje do colorCount generujac nowy colorCount
--      jesli wiersz nie koliduje to powtorz to dla nastepnego wiersza z nowym colorCount
--      jesli wiersz koliduje - wez nastepna permutacje segmentow
-- spermutuj spowrotem wiersze
-- sprawdz czy podane rozwiazanie niesprzeczne z sygnatura kolorow (lista pelna zer obecnie) jest rozwiazaniem
-- jesli nie - kontynuuj dla danego zestawu poczatkowych kombinacji
        
        
--solution parts:
rowOrderByComplexity size rowinfo = map snd $ sort (zip [numberOfCombinations size r | r <- rowinfo] [0..])

buildColorSignature rowinfo = [countAllColors 0 r | r<-rowinfo]
  where
    countAllColors x [] = x
    countAllColors x (r:ri) = countAllColors (fromJust (countColor x (color r) (count r))) ri

--segmentsValidWithSignature (s:segs)

    
-------------------- tools 
numericalTruth True = 1
numericalTruth False = 0

necessaryGaps (a:b:linfo) = numericalTruth ((color a) == (color b)) + (necessaryGaps (b:linfo))
necessaryGaps (c:cc) = 0
necessaryGaps [] = 0

lineTotalLength (x:xs) = count x + lineTotalLength xs
lineTotalLength [] = 0

-- for complexity strategy planning and estimating task complexity
countColor :: Word64 -> Color -> Int -> Maybe Word64
countColor chash color amount = if withinBounds then Just (chash + shiftedAmount)
                                else Nothing
                          where
                            withinBounds = colorValue + amount >= 0 && colorValue + amount < 256
                            shiftBy = (shift (fromEnum color) 3)
                            shiftedAmount = toEnum (shift amount shiftBy)::Word64
                            masked = chash .&. (shift 0xFF shiftBy)
                            colorValue = fromEnum (shift masked (-shiftBy))
  


numberOfCombinations rowSize rowInfo = binom (gapsInRow + segments) (segments)
  where
    gapsInRow = rowSize - (lineTotalLength rowInfo) - (necessaryGaps rowInfo)
    binom n k = product [1+n-k..n] `div` product [1..k]
    segments = length rowInfo

    
    
buildRowFromGaps lineinfo gapinfo = buildRowFromGaps_ White 0 lineinfo gapinfo
buildRowFromGaps_ lastColor endAt (f:rlineinfo) (g:rgapinfo) = 
  if gapLength == 0 then segment : recursive
  else gapsegment : segment: recursive
   where
    extraGap = numericalTruth (lastColor == color f)
    gapLength = extraGap + g
    gapsegment = Segment White (endAt+gapLength)
    segLength = count f
    segEndsAt = segLength+endAt+gapLength
    segment = Segment (color f) segEndsAt
    recursive = buildRowFromGaps_ (color f) segEndsAt rlineinfo rgapinfo
buildRowFromGaps_ lastColor endAt [] (g:rgapinfo) = 
  if g == 0 then []
  else Segment White (endAt+g) : []
    

nextLinePermutation [] bins gaps = gaps : replicate (bins-1) 0
nextLinePermutation perm bins gaps = reverse (carryOne (reverse perm) 0) 
  where
    carryOne (f:s:xs) sumThusFar | s == 0    = case (carryOne (s:xs) (sumThusFar+f)) of
                                                  [] -> []
                                                  carriedOne -> 0:carriedOne
                                 | otherwise = (sumThusFar+f+1):(s-1):xs
    carryOne ls _ = []

    
-- for debuging purposes - a function to display all combinations of nextLinePermutation function
printAllPermutations perm bins gaps = do
  putStrLn $ show perm
  case nextLinePermutation perm bins gaps of
    [] -> putStrLn "Done"
    np -> do 
            printAllPermutations np bins gaps

main = do
  test 0 (necessaryGaps [])
  test 1 (necessaryGaps [BlockInfo {color=Red, count=2}, BlockInfo{color=Red, count=3}])
  test 0 (necessaryGaps [BlockInfo {color=Red, count=2}, BlockInfo{color=Yellow, count=3}])
  test 10 (numberOfCombinations 8 [BlockInfo {color=Red, count=2}, BlockInfo {color=Red, count=2}])
  test [3,0,0] (nextLinePermutation [] 3 3)
  test [2,0,1] (nextLinePermutation [2,1,0] 3 3)
  test [0,3,0] (nextLinePermutation [1,0,2] 3 3)
  test [] (nextLinePermutation [0,0,3] 3 3)
  test [Segment {c = White, endsAt = 1},Segment {c = Red, endsAt = 3},Segment {c = White, endsAt = 6},Segment {c = Red, endsAt = 9}] (buildRowFromGaps [BlockInfo Red 2, BlockInfo Red 3] [1, 2, 0])
  putStrLn "tests passed"

  args <- getArgs
  line <- readFile (head args)
  let puzzle = read line :: Puzzle 
  putStrLn $ show $ solve puzzle

test expect expr = 
  if expect == expr
  then putChar '.'
  else putStrLn $ (show expect ++" != "++ show expr)
  
  
testPuzzle = Puzzle {width = 10, height = 10,
  leftInfo = [
    [BlockInfo {color = Black, count = 2}],
    [BlockInfo {color = Black, count = 1},BlockInfo {color = Blue, count = 1},BlockInfo {color = Black, count = 2}],
    [BlockInfo {color = Red, count = 1},BlockInfo {color = Black, count = 4}],
    [BlockInfo {color = Black, count = 2}],
    [BlockInfo {color = Black, count = 5}],
    [BlockInfo {color = Black, count = 2},BlockInfo {color = Black, count = 3}],
    [BlockInfo {color = Black, count = 3},BlockInfo {color = Black, count = 3}],
    [BlockInfo {color = Black, count = 7}],
    [BlockInfo {color = Red, count = 1},BlockInfo {color = Black, count = 4}],
    [BlockInfo {color = Red, count = 2},BlockInfo {color = Black, count = 2}]
    ],
  upperInfo = [
    [BlockInfo {color = Red, count = 1}],
    [BlockInfo {color = Black, count = 3}, BlockInfo {color = Black, count = 2}],
    [BlockInfo {color = Black, count = 6}, BlockInfo {color = Blue, count = 1}, BlockInfo {color = Black, count = 1}],
    [BlockInfo {color = Red, count = 1}, BlockInfo {color = Black, count = 2}, BlockInfo {color = Black, count = 5}],
    [BlockInfo {color = Red, count = 2}, BlockInfo {color = Black, count = 1}, BlockInfo {color = Black, count = 2}, BlockInfo {color = Black, count = 2}],
    [BlockInfo {color = Black, count = 4}],
    [BlockInfo {color = Black, count = 4}],
    [BlockInfo {color = Black, count = 3}],
    [BlockInfo {color = Black, count = 3}],
    [BlockInfo {color = Black, count = 2}]    
    ]
  }

-- generator wierszy
-- sprawdzacz wierszy
-- sprawdzacz planszy
-- wydruk planszy 