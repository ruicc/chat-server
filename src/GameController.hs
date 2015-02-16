module GameController (spawnControlThread) where

import           App.Prelude
import qualified Log
import           Types

spawnControlThread :: Server -> Group -> IO ()
spawnControlThread srv@Server{..} gr@Group{..} = do
    void $ forkIO $ do
        threadDelay $ 1 * 1000 * 1000

        mask $ \ restore -> do
            tid <- myThreadId
            atomically $ do
                sendBroadcast gr (Command "!begin")
                putTMVar groupGameController tid
                changeGameState gr Playing
            logger $ "Group<" <> expr groupId <> "> Game begins!"

            restore (playGame srv gr) `catch` \ (e :: SomeException) -> do
                -- Cleanup
                onRemove <- atomically $ do
                    changeGameState gr GroupDeleted
                    deleteGroup srv gr
                onRemove
                errorCollector e
                logger $ "An Error occured on playing!"

playGame :: Server -> Group -> IO ()
playGame srv@Server{..} gr@Group{..} = do

    threadDelay $ groupPlayTime * 1000 * 1000
    onRemove <- atomically $ do
        sendBroadcast gr (Command "!finish")
        changeGameState gr GroupDeleted
        deleteGroup srv gr
    onRemove
    logger $ "Group<" <> expr groupId <> "> Game finished!"
