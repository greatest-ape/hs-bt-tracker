module Utils (
    getConfigField,
    getTorrentMap,
    withTorrentMap,
    getsConnectionMap,
    withConnectionMap,
    getThreadIds,
    withThreadIds,
    signalExit,
    waitForExit,
    getTimestamp,
    forkAppM,
    threadDelaySeconds,
    doNTimes
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types


-- * Helpers for server state fields including STM TVars

getConfigField f = f <$> asks _config


getTorrentMap = (\(TorrentMap tm) -> tm) <$> getStateSTMField _torrentMap

withTorrentMap f = withStateSTMField _torrentMap $ g f
    where g f (TorrentMap tm) = TorrentMap $ f tm


getsConnectionMap f =
    (\(ConnectionMap cm) -> f cm) <$> getStateSTMField _connectionMap

withConnectionMap f = withStateSTMField _connectionMap $ g f
    where g f (ConnectionMap cm) = ConnectionMap $ f cm


getThreadIds :: AppM [ThreadId]
getThreadIds = asks _threadIds >>= liftIO . STM.atomically . STM.readTVar

withThreadIds f = withStateSTMField _threadIds f


signalExit :: AppM ()
signalExit = asks _exit >>= liftIO . flip MVar.putMVar ()

waitForExit :: AppM ()
waitForExit = asks _exit >>= liftIO . MVar.takeMVar


getStateSTMField :: (State -> STM.TVar a) -> AppM a
getStateSTMField _accessor = do
    tVar <- asks _accessor
    liftIO $ STM.atomically $ STM.readTVar tVar

withStateSTMField :: (State -> STM.TVar a) -> (a -> a) -> AppM ()
withStateSTMField _accessor f = do
    tVar <- asks _accessor
    liftIO $ STM.atomically $ STM.modifyTVar tVar f


-- * Other helpers

getTimestamp :: IO Timestamp
getTimestamp = Timestamp . round <$> getPOSIXTime


forkAppM :: AppM () -> AppM ThreadId
forkAppM f = ask >>= liftIO . forkIO . runReaderT f


threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n = threadDelay $ 1000000 * n


doNTimes :: Integer -> AppM a -> AppM ()
doNTimes 0 f = return ()
doNTimes n f = f >> doNTimes (n - 1) f
