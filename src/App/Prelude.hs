module App.Prelude
    ( module P
    , module Control.Applicative
    , module Control.Monad
--    , module Control.Concurrent
--    , module Control.Concurrent.STM
--    , module Control.Concurrent.Async
--    , module Control.Exception
    , module Data.Monoid
    -- * ShortByteString
    , ShortByteString, toShort, fromShort
    , SB.null
    -- * Show
    , expr, show
    -- * System.IO
    , putStr, putStrLn
    , hPutStr, hPutStrLn, hGetLine
    , IO.Handle , IO.BufferMode(..)
    , hFlush, hClose , hSetBuffering
    , words, rstrip
    , readInt
    -- * Conversion between Text and ShortByteString
    , toText, fromText
    , liftIO
    )
    where

import           Prelude as P hiding (log, lookup, putStrLn, putStr, words, null, show)
import qualified Prelude as P (show)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
--import           Control.Concurrent
--import           Control.Concurrent.STM
--import           Control.Concurrent.Async
--import           Control.Exception hiding (mask, catch)

import           Data.Monoid
import           Data.Char (isSpace)
--import qualified Data.ByteString as B hiding (putStrLn, putStr)
import qualified Data.ByteString.Char8 as B.Char
import           Data.ByteString.Short (ShortByteString, toShort, fromShort)
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Binary as Bi
import qualified Data.ByteString.Lazy as BSL

import qualified System.IO as IO

expr :: Show a => a -> ShortByteString
expr = toShort . B.Char.pack . P.show
-- TODO: Don't go through String
--expr :: Bi.Binary a => a -> ShortByteString
--expr = toShort . BSL.toStrict . Bi.encode

show :: Show a => a -> ShortByteString
show = toShort . B.Char.pack . P.show

putStrLn :: ShortByteString -> IO ()
putStrLn = B.Char.putStrLn . fromShort

putStr :: ShortByteString -> IO ()
putStr = B.Char.putStr . fromShort

hPutStr :: IO.Handle -> ShortByteString -> IO ()
hPutStr hdl sb = B.Char.hPutStr hdl (fromShort sb)

hPutStrLn :: IO.Handle -> ShortByteString -> IO ()
hPutStrLn hdl sb = B.Char.hPutStrLn hdl (fromShort sb)

hGetLine :: IO.Handle -> IO ShortByteString
hGetLine hdl = toShort <$> (B.Char.hGetLine hdl)

hFlush :: IO.Handle -> IO ()
hFlush = IO.hFlush

hClose :: IO.Handle -> IO ()
hClose = IO.hClose

hSetBuffering :: IO.Handle -> IO.BufferMode -> IO ()
hSetBuffering h bm = IO.hSetBuffering h bm



words :: ShortByteString -> [ShortByteString]
words sb = toShort <$> (B.Char.words $ fromShort $ sb)

rstrip :: ShortByteString -> ShortByteString
rstrip = toShort . B.Char.reverse . B.Char.dropWhile isSpace . B.Char.reverse . fromShort

readInt :: ShortByteString -> Maybe (Int, ShortByteString)
readInt sb = fmap (\ (i,b) -> (i, toShort b)) $ B.Char.readInt $ fromShort sb

null :: ShortByteString -> Bool
null = B.Char.null . fromShort

------------------------------------------------------------------------------------------
-- | Text and ShortByteString
--
-- Text is used in JSON or something UTF8-encoded text.

toText :: ShortByteString -> T.Text
toText = decodeUtf8 . fromShort

fromText :: T.Text -> ShortByteString
fromText = toShort . encodeUtf8
