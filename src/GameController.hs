module GameController (spawnControlThread) where

import           App.Prelude as P
--import qualified Control.Concurrent as Conc
import qualified Log
import           Types
import           Concurrent

spawnControlThread :: Server -> Group -> Concurrent ()
spawnControlThread srv@Server{..} gr@Group{..} = do
    void $ forkC $ do
        threadDelay $ 1 * 1000 * 1000

        mask $ \ restore -> do
            tid <- myThreadId
            runSTM $ do
                sendBroadcast gr (Command "!begin")
                putTMVar groupGameController tid
                changeGameState gr Playing
            liftIO $ logger $ "Group<" <> expr groupId <> "> Game begins!"

            restore (playGame srv gr) `catch` \ (e :: SomeException) -> do
                -- Cleanup
                onRemove <- runSTM $ do
                    changeGameState gr GroupDeleted
                    deleteGroup srv gr
                liftIO onRemove
                liftIO $ errorCollector e
                liftIO $ logger $ "An Error occured on playing!"

playGame :: Server -> Group -> Concurrent ()
playGame srv@Server{..} gr@Group{..} = do

    threadDelay $ groupPlayTime * 1000 * 1000
    onRemove <- runSTM $ do
        sendBroadcast gr (Command "!finish")
        changeGameState gr GroupDeleted
        deleteGroup srv gr
    liftIO onRemove
    liftIO $ logger $ "Group<" <> expr groupId <> "> Game finished!"
