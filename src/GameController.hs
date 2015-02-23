module GameController (spawnControlThread) where

import           App.Prelude as P
--import qualified Control.Concurrent as Conc
import qualified Log
import           Types
import           Concurrent

spawnControlThread :: Server -> Group -> Concurrent ()
spawnControlThread srv@Server{..} gr@Group{..} = do
    void $ fork_ $ do
        threadDelay $ 1 * 1000 * 1000

        mask $ \ restore -> do
            tid <- myThreadId
            atomically_ $ do
                sendBroadcast gr (Command "!begin")
                putTMVar groupGameController tid
                changeGameState gr Playing
            logger srv $ "Group<" <> expr groupId <> "> Game begins!"

            restore (playGame srv gr) `catch` \ (e :: SomeException) -> do
                -- Cleanup
                onRemove <- atomically_ $ do
                    changeGameState gr GroupDeleted
                    deleteGroup srv gr
                onRemove
                errorCollector srv e
                logger srv $ "An Error occured on playing!"

playGame :: Server -> Group -> Concurrent ()
playGame srv@Server{..} gr@Group{..} = do

    threadDelay $ groupPlayTime * 1000 * 1000
    onRemove <- atomically_ $ do
        sendBroadcast gr (Command "!finish")
        changeGameState gr GroupDeleted
        deleteGroup srv gr
    onRemove
    logger srv $ "Group<" <> expr groupId <> "> Game finished!"
