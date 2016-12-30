module Types.Server where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.MVar as MVar
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Int (Int32)
import Data.Word (Word16)

import Types.Common
import Types.Peer


data Config = Config {
    _serverAddress           :: String,
    _serverPort              :: Word16,
    _numberOfThreads         :: Int,

    _announceInterval        :: Int32,
    _maximumPeersToSend      :: Int,

    _connectionMaxAge        :: Integer,
    _connectionPruneInterval :: Int,

    _peerMaxAge              :: Integer,
    _peerPruneInterval       :: Int
}

newtype TorrentMap = TorrentMap (Map.HashMap InfoHash (Sequence.Seq Peer))
    deriving (Show)

data ConnectionMapKey = ConnectionMapKey !ConnectionID !IPAddress
    deriving (Show, Eq, Ord)

instance Hashable ConnectionMapKey where
    hashWithSalt salt (ConnectionMapKey (ConnectionID n) (IPAddress m)) = hashWithSalt salt (n, m)

newtype ConnectionMap = ConnectionMap (Map.HashMap ConnectionMapKey Timestamp)
    deriving (Show)

data State = State {
    _config        :: Config,
    _torrentMap    :: STM.TVar TorrentMap,
    _connectionMap :: STM.TVar ConnectionMap,
    _threadIds     :: STM.TVar [ThreadId],
    _exit          :: MVar.MVar ()
}

type AppM = ReaderT State IO

