import System.Console.ANSI

palette :: [Color]
palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]

putCharWithColor :: Char -> Color -> IO ()
putCharWithColor x c = do
    setSGR [SetColor Foreground Vivid c]
    putChar x
    setSGR [Reset]

test :: [Color] -> IO ()
test []     = putChar '\n'
test (c:cs) = do putCharWithColor 'X' c
                 test cs

main = do test palette
