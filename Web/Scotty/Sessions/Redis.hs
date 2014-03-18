{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Sessions.Redis
( redisBackend
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Default
import Control.Monad
import qualified Database.Redis as R
import System.Random
import Network.Wai
import Web.Cookie (parseCookies, renderSetCookie, SetCookie(..))
import qualified Blaze.ByteString.Builder as B
import qualified Data.Vault.Lazy as Vault
import Data.String
import Network.Wai.Internal (Response(ResponseBuilder,ResponseFile,ResponseSource))
import Network.HTTP.Types (ResponseHeaders)
import Web.Scotty.Sessions

setSessionExpiring :: R.Connection -> ByteString -> Integer -> [(ByteString, ByteString)] -> IO ()
setSessionExpiring conn key timeout values = R.runRedis conn $ do
    R.del [key]
    forM_ values (\x -> R.hset key (fst x) (snd x))
    R.expire key timeout
    return ()

getSession :: R.Connection -> ByteString -> IO (Maybe [(ByteString, ByteString)])
getSession conn key = R.runRedis conn $ do
    result <- R.hgetall key
    let output = case result of
                     (Right b) -> Just b
                     _ -> Nothing
    return output

redisBackend :: R.Connection -> Integer -> Backend
redisBackend conn timeout key = (getSession conn key, setSessionExpiring conn key timeout)
