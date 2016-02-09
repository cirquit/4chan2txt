module HTTPAction where

import           Network.HTTP.Conduit        (simpleHttp)
import qualified Data.ByteString.Lazy  as LB (ByteString(..), empty)
import           Control.Exception     as EX (catch, SomeException())
import           Data.Aeson                  (eitherDecode)

import ChanTypes                              (Thread(..), Page(..))

-- | fetches all the threadids from all the pages
--
getBoardInfo :: String -> IO (Either String [Page])
getBoardInfo board = decodeBoardInfo <$> safeHttp link
    where link = "http://a.4cdn.org/" ++ board ++ "/threads.json"

-- | fetches a thread via threadid
--
getThreadByID :: String -> Integer -> IO (Either String Thread)
getThreadByID board id = decodeThread <$> safeHttp link
    where link = "http://a.4cdn.org/" ++ board ++ "/thread/" ++ show id ++ ".json"

-- | catches every possible exception and returns an empty bytestring
--   
safeHttp :: String -> IO LB.ByteString
safeHttp l = simpleHttp l `EX.catch` statusExceptionHandler
    where
        statusExceptionHandler :: SomeException -> IO LB.ByteString
        statusExceptionHandler e = putStrLn ("error: " ++ take 76 (show e)) >> return LB.empty

decodeThread :: LB.ByteString -> Either String Thread
decodeThread = eitherDecode

decodeBoardInfo :: LB.ByteString -> Either String [Page]
decodeBoardInfo = eitherDecode