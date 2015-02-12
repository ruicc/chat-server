module App.Time
    ( UnixTime(..), CTime(..)
    , getUnixTime, unixtimeToInt
    , getUnixTimeAsInt
    ) where

import           App.Prelude
import           Control.Applicative

import           Data.UnixTime (getUnixTime, UnixTime(..))
import           Foreign.C.Types (CTime(..))


getUnixTimeAsInt :: IO Int
getUnixTimeAsInt = unixtimeToInt <$> getUnixTime

-- Ignore after the decimal point
unixtimeToInt :: UnixTime -> Int
unixtimeToInt (UnixTime (CTime t) _) = fromIntegral t

