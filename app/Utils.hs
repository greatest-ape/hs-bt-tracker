module Utils (
    getConfigField,
    getTorrentMap,
    withTorrentMap,
    getsConnectionMap,
    withConnectionMap,
    getThreadIds,
    withThreadIds,
    getTimestamp
) where

import qualified Control.Concurrent.STM as STM

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types


-- * Helpers for server state fields including STM TVars

getConfigField f = f <$> asks _config


getTorrentMap = do
    (TorrentMap tm) <- getStateSTMField _torrentMap
    return tm

withTorrentMap f = withStateSTMField _torrentMap $ g f
    where g f (TorrentMap tm) = TorrentMap $ f tm


getsConnectionMap f = do
    (ConnectionMap cm) <- getStateSTMField _connectionMap
    return $ f cm

withConnectionMap f = withStateSTMField _connectionMap $ g f
    where g f (ConnectionMap cm) = ConnectionMap $ f cm


getThreadIds :: AppM [ThreadId]
getThreadIds = asks _threadIds >>= liftIO . STM.atomically . STM.readTVar

withThreadIds f = withStateSTMField _threadIds f


getStateSTMField :: (State -> STM.TVar a) -> AppM a
getStateSTMField _accessor = do
    tVar <- asks _accessor
    liftIO $ STM.atomically $ STM.readTVar tVar

withStateSTMField :: (State -> STM.TVar a) -> (a -> a) -> AppM ()
withStateSTMField _accessor f = do
    tVar <- asks _accessor
    liftIO $ STM.atomically $ STM.modifyTVar tVar f


-- * Other helpers

getTimestamp :: IO TimeStamp
getTimestamp = TimeStamp . round <$> getPOSIXTime
