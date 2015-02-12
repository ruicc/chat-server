{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative
import           Foreign.C.Types
import           Data.Char (toLower)
import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import qualified Data.UnixTime as U
import qualified App.Time as Time
import qualified App.Types.ClientMessage as CM

data User01
    = User01  -- User data for ver 0.1 
    { user01Name  :: Text
    , user01Age   :: Int      -- This column is removed from ver 0.2
    , user01Email :: Text
    , user01Timestamp :: Int
    }
  deriving (Show, Read, Eq)

data User02
    = User02  -- User data for ver 0.2
    { user02Name    :: Text
    , user02Address :: Text   -- This column is added from ver 0.2
    , user02Email   :: Text
    }
  deriving (Show, Read, Eq)

deriveJSON defaultOptions { fieldLabelModifier = map toLower . drop 6 } ''User01
deriveJSON defaultOptions { fieldLabelModifier = map toLower . drop 6 } ''User02



main = do
    ts <- Time.unixtimeToInt <$> Time.getUnixTime

    let
        mu1 :: Maybe User01
            = decode $ encode $ User01 "アリス" 23 "alice@example.com" ts
        mu2 :: Maybe User02
            = decode $ encode $ User02 "ボブ" "xxx.yyy" "bob@example.com"

        died :: Maybe CM.ClientMessage = decode "{\"tag\":\"Died\",\"contents\":[]}"
        died2 :: Maybe CM.ClientMessage = decode "{\"tag\":\"Died\"}"

--    print mu1
--    print mu2

    BL.putStrLn $ encode $ CM.CreateRoom "Alice and her followers" 3 (5 * 60)
    BL.putStrLn $ encode $ CM.JoinRoom 42
    BL.putStrLn $ encode $ CM.UserConfig CM.Red "alic"
    BL.putStrLn $ encode $ CM.Died
    print $ died
    print $ died2


