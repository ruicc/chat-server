{-# LANGUAGE TemplateHaskell #-}

import           Data.Char (toLower)
import           Data.Aeson (encode)
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T

data User01
    = User01  -- User data for ver 0.1 
    { u1Name  :: Text
    , u1Age   :: Int      -- This column is removed from ver 0.2
    , u1Email :: Text
    }
  deriving (Show, Read, Eq)

data User02
    = User02  -- User data for ver 0.2
    { u2Name    :: Text
    , u2Address :: Text   -- This column is added from ver 0.2
    , u2Email   :: Text
    }
  deriving (Show, Read, Eq)

deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 2
--    , allNullaryToStringTag = True
--    , omitNothingFields = True
--    , sumEncoding = ObjectWithSingleField
    , sumEncoding = TwoElemArray
    } ''User01
deriveJSON defaultOptions { fieldLabelModifier = map toLower . drop 2 } ''User02

main = do
    print $ encode $ User01 "アリス" 23 "alice@hoge.com"
    print $ encode $ User02 "ボブ" "xxx.yyy" "bob@hoge.com"
