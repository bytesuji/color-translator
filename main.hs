import Data.List
import System.IO
import System.Environment


main = do -- FIXME: handle no such file, too few args
    (inputFile:_) <- getArgs
    contents <- readFile inputFile 
    putStrLn $ "Generated profile:\n\n" ++ produceFullProfile contents


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


genericPlusSearch :: String -> Int -> String -> String
genericPlusSearch searchTerm plus fileContents = 
    let splitContents = words fileContents
        resultIndex = case (elemIndex searchTerm splitContents) of
            Just index -> index + plus
            Nothing -> (-1) -- FIXME
    in splitContents !! resultIndex
    
    
producePalette :: String -> String
producePalette fileContents = 
    intercalate ":" . take 2 . repeat . intercalate ":" $ colors
    where colors = map (getColor fileContents) [0..7] 
    -- this is my masterpiece


produceFullProfile :: String -> String
produceFullProfile fileContents = 
    "[[" ++ name ++ "]]\n" ++ 
    "  background_color = \"" ++ bg ++ "\"\n"   ++ 
    "  cursor_color = \"" ++ fg ++ "\"\n"       ++ 
    "  font = $YOUR_FONT_HERE\n"                ++ 
    "  foreground_color = \"" ++ fg ++ "\"\n"   ++ 
    "  palette = \"" ++ palette ++ "\"\n"       ++
    "  scrollbar_position = hidden\n"           ++
    "  show_titlebar = false\n"                 ++
    "  use_system_font = False"
    where 
    name = "Base16-" ++ genericPlusSearch "Base16" 1 fileContents
    bg = (flip getBase) fileContents . genericPlusSearch "*.background:" 4 $ fileContents
    fg = (flip getBase) fileContents . genericPlusSearch "*.foreground:" 1 $ fileContents
    palette = producePalette fileContents
