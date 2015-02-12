module App.Prelude
    ( module P
--    , module B
    , module B.Short
    , module Control.Applicative
    , module Control.Monad
    , module Control.Concurrent
    , module Control.Concurrent.STM
    , module Control.Concurrent.Async
    , module Control.Exception
    , module Data.Monoid
    , expr
    , putStr, putStrLn
    , hPutStr, hPutStrLn, hGetLine
    , IO.Handle, IO.hFlush
    , IO.hSetBuffering
    , IO.BufferMode(..)
    , words, rstrip
    , readInt
    )
    where

import           Prelude as P hiding (log, lookup, putStrLn, putStr, words)
import           Control.Applicative
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception

import           Data.Monoid
import           Data.Char (isSpace)
--import qualified Data.ByteString as B hiding (putStrLn, putStr)
import qualified Data.ByteString.Char8 as B.Char
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B.Short

import qualified System.IO as IO

expr :: Show a => a -> ShortByteString
expr = B.Short.toShort . B.Char.pack . P.show

putStrLn :: ShortByteString -> IO ()
putStrLn = B.Char.putStrLn . B.Short.fromShort

putStr :: ShortByteString -> IO ()
putStr = B.Char.putStr . B.Short.fromShort

hPutStr :: IO.Handle -> ShortByteString -> IO ()
hPutStr hdl sb = B.Char.hPutStr hdl (B.Short.fromShort sb)

hPutStrLn :: IO.Handle -> ShortByteString -> IO ()
hPutStrLn hdl sb = B.Char.hPutStrLn hdl (B.Short.fromShort sb)

hGetLine :: IO.Handle -> IO ShortByteString
hGetLine hdl = B.Short.toShort <$> B.Char.hGetLine hdl

words :: ShortByteString -> [ShortByteString]
words sb = B.Short.toShort <$> (B.Char.words $ B.Short.fromShort $ sb)

rstrip :: ShortByteString -> ShortByteString
rstrip = B.Short.toShort . B.Char.reverse . B.Char.dropWhile isSpace . B.Char.reverse . B.Short.fromShort

readInt :: ShortByteString -> Maybe (Int, ShortByteString)
readInt sb = fmap (\ (i,b) -> (i, B.Short.toShort b)) $ B.Char.readInt $ B.Short.fromShort sb



