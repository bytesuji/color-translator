import Data.List
import System.IO
import System.Exit
import System.Environment
import Control.Exception

main = do
    (inputFile:_) <- getArgs
    contents <-
        catch (readFile inputFile)
              (\e -> do let err = show (e :: IOException)
                        die ("Warning: Could not open " ++ inputFile ++ ": " ++ err)) 
    let profile = produceFullProfile contents
    putStrLn $ "Generated profile:\n\n" ++ produceFullProfile contents


getColor :: String -> Int -> String
getColor fileContents n = 
    let splitContents = words fileContents 
        searchString = "*.color" ++ (show n) ++ ":"
        baseIndex = case (elemIndex searchString splitContents) of
            Just index -> index + 1
            Nothing -> (-1) --- FIXME: actual error handling
    in getBase (splitContents !! baseIndex) fileContents


genericPlusSearch :: String -> Int -> String -> String
genericPlusSearch searchTerm plus fileContents = 
    let splitContents = words fileContents
        resultIndex = case (elemIndex searchTerm splitContents) of
            Just index -> index + plus
            Nothing -> (-1) -- FIXME
    in splitContents !! resultIndex


getBase :: String -> String -> String
getBase baseName fileContents = genericPlusSearch baseName 1 fileContents
    
    
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
