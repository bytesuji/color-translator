import Data.List
import System.IO
import System.Environment


main = do -- FIXME: handle no such file, too few args
    (inputFile:_) <- getArgs
    contents <- readFile inputFile 
    putStrLn $ producePalette contents


getColor :: String -> Int -> String
getColor fileContents n = 
    let splitContents = words fileContents 
        searchString = "*.color" ++ (show n) ++ ":"
        baseIndex = case (elemIndex searchString splitContents) of
            Just index -> index + 1
            Nothing -> (-1) --- FIXME: actual error handling
    in getBase (splitContents !! baseIndex) fileContents


getBase :: String -> String -> String
getBase baseName fileContents =
    let splitContents = words fileContents 
        colorIndex = case (elemIndex baseName splitContents) of
            Just index -> index + 1
            Nothing -> (-1) --- FIXME: actual error handling    
        in splitContents !! colorIndex
    

producePalette :: String -> String
producePalette fileContents = 
    intercalate ":" . take 2 . repeat . intercalate ":" $ colors
    where colors = map (getColor fileContents) [0..7] 
    -- this is my masterpiece
