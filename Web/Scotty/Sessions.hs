{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Sessions
( session
, Backend
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

newKey = do
        let n = 30
        gen <- newStdGen
        let chars = ['0'..'9']++['a'..'z']++['A'..'B']
        let numbers = randomRs (0, (length chars - 1)) gen
        return $ BS.pack $ take n $ map (chars!!) numbers

type Backend = ByteString -> (IO (Maybe [(ByteString, ByteString)]), 
    ([(ByteString, ByteString)] -> IO ()))

session :: Backend -> Vault.Key ([(ByteString, ByteString)], 
    ([(ByteString,ByteString)] -> IO ())) -> Middleware
session backend key app req = do
        let msessid = lookup cookieName =<< cookies
        sessid <- liftIO $ case msessid of
                         Nothing -> do
                            newsessid <- newKey
                            return newsessid
                         Just sessid -> do
                            let (bget, bset) = backend sessid
                            msess <- bget
                            newsessid <- case msess of
                                    Just _ -> return sessid
                                    Nothing -> do
                                        newsessid <- newKey
                                        return newsessid
                            return newsessid
        let (bget, bset) = backend sessid
        msess <- bget
        let sess = case msess of
                       Just sess -> sess
                       Nothing -> []
        let newReq = changeRequest sess bset req
        res <- app newReq
        return $ changeResponse sessid res
    where
        cookieName = "sessionid"
        changeRequest :: [(ByteString,ByteString)] -> 
            ([(ByteString,ByteString)] -> IO ()) -> Request -> Request
        changeRequest sess bset req = 
                req {vault = Vault.insert key (sess, bset) (vault req)}
        setCookie = fromString "Set-Cookie"
        ciCookie = fromString "Cookie"
        changeResponse :: ByteString -> Response -> Response
        changeResponse sessid res = mapHeader (\hs -> 
                            (setCookie, newCookie cookieName sessid):hs) res
        cookies = fmap parseCookies $ lookup ciCookie (requestHeaders req)

newCookie cookieName cookieVal = B.toByteString $ 
        renderSetCookie $ def 
                            { setCookieName = cookieName 
                            , setCookieValue = cookieVal
                            }
mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseSource s h b) = ResponseSource s (f h) b
