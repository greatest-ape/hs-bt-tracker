module Types.Server where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)


data Config = Config {
    _configServerAddress :: String,
    _configNumberOfThreads :: Int
}

newtype TorrentMap = TorrentMap (Map.HashMap Int [Int])
    deriving (Show)

newtype ConnectionMap = ConnectionMap (Map.HashMap Int Int)
    deriving (Show)

data State = State {
    _config        :: Config,
    _torrentMap    :: STM.TVar TorrentMap,
    _connectionMap :: STM.TVar ConnectionMap,
    _threadIds     :: STM.TVar [ThreadId]
}

type AppM = ReaderT State IO

