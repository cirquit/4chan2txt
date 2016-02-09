module Parser where

import ChanTypes  (Thread(..), ThreadInfo(..), Post(..), Page(..))
import Data.Char  (isNumber)
import Data.Text  (unpack)


getThreadIDs :: [Page] -> [Integer]
getThreadIDs = removeSticky . concatMap (map tid . threads)
    where removeSticky = drop 1
          tid = no

threadToText :: Either String Thread -> String
threadToText (Left _)           = ""
threadToText (Right (Thread ps)) = content
    where content = concatMap ((++ "\n") . unpack . com) ps

-- | Todo - use megaparsec
--
filterText :: [String] -> String
filterText = concatMap (++ "\n") . map (filterTags True)
  where filterTags :: Bool -> String -> String
        filterTags _     []                            = []
        filterTags _     ", "                          = []
        filterTags _     ('<':'a':xs)                  = filterTags False xs
        filterTags _     ('<':'/':'a':'>':xs)          = filterTags True  xs
        filterTags _     ('<':'s':'p':'a':'n':xs)         = filterTags False xs
        filterTags _     ('<':'/':'s':'p':'a':'n':'>':xs) = filterTags True  xs
        filterTags b@_   ('<':'b':'r':'>':xs)          = ' ' : filterTags b xs
        filterTags b@_   ('<':'w':'b':'r':'>':xs)      = ' ' : filterTags b xs
        filterTags b@_   ('&':'g':'t':';':xs)          = filterTags b xs
        filterTags b@_   ('&':'l':'t':';':xs)          = filterTags b xs
        filterTags b@_   ('&':'#':'0':'3':'9':';':xs)  = '\'' : filterTags b    xs
        filterTags True  ('&':'q':'u':'o':'t':';': xs) = '"'  : filterTags True xs
        filterTags _     ('&':xs)                      = filterTags False xs
        filterTags _     (';':xs)                      = filterTags True xs
        filterTags True  l@('h':'t':'t':'p':xs)        = filterTags True (unwords (tail (words l)))
        filterTags True  (x:xs)                        = x : filterTags True xs
        filterTags False (_:xs)                        = filterTags False xs