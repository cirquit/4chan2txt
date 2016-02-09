{-# LANGUAGE BangPatterns #-}

import Control.Monad      (forM)
import System.IO          (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getArgs, getProgName)

import Parser             (threadToText, filterText, getThreadIDs)
import HTTPAction         (getBoardInfo, getThreadByID)

saveDataTo :: String -> FilePath -> IO ()
saveDataTo board fp = do
    putStrLn "Fetching board information..."
    binfo <- getBoardInfo board
    case binfo of
        (Left _)      -> return ()
        (Right pages) -> do
            let threadIDs  = getThreadIDs pages
            putStrLn "Fetching threads information..."
            eitherThreads <- forM threadIDs (getThreadByID board)
            let !text = map threadToText eitherThreads
            writeFile fp (filterText text)
            putStrLn $ "Content stored in " ++ fp ++ "!"

allBoards :: [String]
allBoards = ["a", "b", "c", "d", "e", "f", "g", "gif", "h", "hr", "k", "m", "o", "p", "r", "s", "t", "u", "v",
             "vg", "vr", "w", "wg", "i", "ic", "r9k", "s4s", "cm", "hm", "lgbt", "y", "3", "adv", "an", "asp",
             "biz", "cgl", "ck", "co", "diy", "fa", "fit", "gd", "hc", "int", "jp", "lit", "mlp", "mu", "n",
             "out", "po", "pol", "sci", "soc", "sp", "tg", "toy", "trv", "tv", "vp", "wsg", "x"]

main :: IO()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    case args of
        ["-board", board, "-to", fp]
            | board `notElem` allBoards -> putStrLn "Sorry, this board can not be found."
            | otherwise                 -> board `saveDataTo` fp
        (_)                             -> do
          name <- getProgName
          putStrLn "How to use:"
          putStrLn $ "./" ++ name ++ " -board <4chanboard> -to <filepath to .txt>"
