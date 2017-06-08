import Data.List
import Data.List.Split
import System.IO
import System.Environment


main = do -- FIXME: handle no such file, too few args
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    let color0 = getColor 0 contents 
    print color0


getColor :: Int -> String -> String
getColor n fileContents = 
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
