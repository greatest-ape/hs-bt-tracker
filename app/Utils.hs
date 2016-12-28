module Utils (
    getConfigField,
    withTorrentMap,
    withConnectionMap,
    withThreadIds,
    getThreadIds
) where

import qualified Control.Concurrent.STM as STM

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)

import Types.Server


getConfigField f = f <$> asks _config


withTorrentMap f = withStateSTMField _torrentMap $ g f
    where g f (TorrentMap tm) = TorrentMap $ f tm


withConnectionMap f = withStateSTMField _connectionMap $ g f
    where g f (ConnectionMap cm) = ConnectionMap $ f cm


withThreadIds f = withStateSTMField _threadIds f


getThreadIds :: AppM [ThreadId]
getThreadIds = asks _threadIds >>= liftIO . STM.atomically . STM.readTVar


withStateSTMField :: (State -> STM.TVar a) -> (a -> a) -> AppM ()
withStateSTMField _accessor f = do
    tVar <- asks _accessor
    liftIO $ STM.atomically $ STM.modifyTVar tVar f

