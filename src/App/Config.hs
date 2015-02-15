module App.Config
    ( getConfig
    , Config(..), ServerConfig(..), EkgConfig(..), ClientConfig(..)
    ) where

import           App.Prelude
import           Data.Yaml (decodeFile)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Scientific as S
import           Data.HashMap.Strict (lookup)
import qualified Data.ByteString.Char8 as B

data Config = Config
    { serverConfig :: ServerConfig
    , ekgConfig :: EkgConfig
    , clientConfig :: ClientConfig
    }
data ServerConfig = ServerConfig
    { serverMaxConnections :: Int
    , serverHost :: String
    , serverPort :: Int
    }
data EkgConfig = EkgConfig
    { ekgHost :: B.ByteString
    , ekgPort :: Int
    }
data ClientConfig = ClientConfig
    { clientSpawnThreads :: Int
    }

getConfig :: FilePath -> IO Config
getConfig path = do
    let
        getObj (A.Object v) = Just v
        getObj _ = Nothing
        getStr (A.String v) = Just $ T.unpack v
        getStr _ = Nothing
        getBS (A.String v) = Just $ B.pack $ T.unpack v
        getBS _ = Nothing
        getNum (A.Number v) = Just $ fromInteger $ S.coefficient v
        getNum _ = Nothing

    mYaml <- decodeFile path

    maybe (error $ path <> " is wrong") (return) $ do
        settings
            <- getObj =<< mYaml

        server
            <- getObj =<< lookup "server" settings
        ekg
            <- getObj =<< lookup "ekg" settings
        client
            <- getObj =<< lookup "client" settings

        -- server
        serverMaxConnections
            <- getNum =<< lookup "max_connections" server
        serverHost
            <- getStr =<< lookup "host" server
        serverPort
            <- getNum =<< lookup "port" server

        -- ekg
        ekgHost
            <- getBS =<< lookup "host" ekg
        ekgPort
            <- getNum =<< lookup "port" ekg
        
        -- client
        clientSpawnThreads
            <- getNum =<< lookup "spawn_threads" client

        return $ Config
            (ServerConfig serverMaxConnections serverHost serverPort)
            (EkgConfig ekgHost ekgPort)
            (ClientConfig clientSpawnThreads)
