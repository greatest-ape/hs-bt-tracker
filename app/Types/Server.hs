module Types.Server where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)


newtype TorrentMap = TorrentMap (Map.HashMap Int [Int])
    deriving (Show)

newtype ConnectionMap = ConnectionMap (Map.HashMap Int Int)
    deriving (Show)

data State = State {
    _torrentMap    :: STM.TVar TorrentMap,
    _connectionMap :: STM.TVar ConnectionMap,
    _threadIds     :: STM.TVar [ThreadId]
}

type AppM = ReaderT State IO

