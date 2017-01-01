-- Request and related data types
module Types.Request where

import qualified Data.Sequence as Sequence

import Data.Int (Int32, Int64)

import Types.Common


data Request
    = ConnectRequest ConnectRequestInner
    | AnnounceRequest AnnounceRequestInner
    | ScrapeRequest ScrapeRequestInner
    | InvalidRequest


data ConnectRequestInner = ConnectRequestInner {
    _transactionID    :: !TransactionID
} deriving (Show)


data AnnounceRequestInner = AnnounceRequestInner {
    _connectionID    :: !ConnectionID,
    _transactionID   :: !TransactionID,
    _infoHash        :: !InfoHash,
    _peerID          :: !PeerID,
    _bytesDownloaded :: !BytesDownloaded,
    _bytesUploaded   :: !BytesUploaded,
    _bytesLeft       :: !BytesLeft,
    _announceEvent   :: !AnnounceEvent,
    _ipAddress       :: !IPAddress,
    _key             :: !PeerKey,
    _peersWanted     :: !PeersWanted,
    _port            :: !PeerPort
} deriving (Show)


data ScrapeRequestInner = ScrapeRequestInner {
    _connectionID      :: !ConnectionID,
    _transactionID     :: !TransactionID,
    _infoHashes        :: !(Sequence.Seq InfoHash)
} deriving (Show)


data AnnounceEvent
    = AnnounceEventCompleted
    | AnnounceEventStarted
    | AnnounceEventStopped
    | AnnounceEventNone

    deriving (Show, Eq)

newtype BytesDownloaded = BytesDownloaded Int64
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype BytesLeft = BytesLeft Int64
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype BytesUploaded = BytesUploaded Int64
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype PeersWanted = PeersWanted Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
