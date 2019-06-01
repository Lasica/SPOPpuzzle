module PutBoard where

import System.Console.ANSI as ANSI

-- board = [[Blue, Blue],[Blue, Blue]]

putBoard board = do 
  mapM_ drawBoard board

drawBoard line = do
  mapM_ drawField line
  newNormalLine

newNormalLine = do
  setSGR [Reset]
  putStrLn " "

drawField :: Color -> IO ()
drawField field = do
  setSGR $ getFieldSGR field
  putChar ' '

getFieldSGR :: Color -> [SGR]
getFieldSGR field = [ SetColor Background Vivid field  ]