import Data.List
import Data.List.Split
import System.IO
import System.Environment


main = do
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    --print $ words contents 
    let color0 = getColor 0 contents 
    print color0


getColor :: Int -> String -> String
getColor n fileContents = 
    let splitContents = words fileContents 
        searchString = "*.color:" ++ (show n)
        colorIndex = case (elemIndex searchString splitContents) of
            Just index -> index + 1
            Nothing -> 0
    in searchString
