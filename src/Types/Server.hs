module Types.Server where

import           App.Prelude
import           Control.Concurrent.Structured
import qualified Data.IntMap as IM

import Log
import Types.Group


data Server = Server
    { serverGroups :: TVar (IM.IntMap Group)
    , logChan :: Log.LogChan
    , statChan :: Log.StatChan
    , errorChan :: Log.ErrorChan
    }

newServer :: Log.LogChan -> Log.StatChan -> Log.ErrorChan -> IO Server
newServer logCh statCh erCh = runCIO return $ do
    let
    gs <- newTVarCIO IM.empty

    return $ Server gs logCh statCh erCh

logger :: Server -> ShortByteString -> CIO r ()
#if DEVELOPMENT
logger Server{..} sb = atomically_ $ writeTChan logChan sb
#else
logger Server{..} sb = return ()
#endif

tick :: Server -> Log.AppEvent -> CIO r ()
tick Server{..} ev = atomically_ $ writeTChan statChan ev

errorCollector :: Server -> SomeException -> CIO r ()
errorCollector Server{..} e = atomically_ $ writeTChan errorChan e
