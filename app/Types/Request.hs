{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- Request and related data types
module Types.Request where

import Data.Int (Int32, Int64)
import Data.Word (Word32)

import Types.Common


data Request
    = ConnectRequest ConnectRequestInner
    | AnnounceRequest AnnounceRequestInner
    | ScrapeRequest ScrapeRequestInner
    | InvalidRequest


data ConnectRequestInner = ConnectRequestInner {
    _connectRequestTransactionID    :: !TransactionID
} deriving (Show)


data AnnounceRequestInner = AnnounceRequestInner {
    _announceRequestConnectionID    :: !ConnectionID,
    _announceRequestTransactionID   :: !TransactionID,
    _announceRequestInfoHash        :: !InfoHash,
    _announceRequestPeerID          :: !PeerID,
    _announceRequestBytesDownloaded :: !BytesDownloaded,
    _announceRequestBytesUploaded   :: !BytesUploaded,
    _announceRequestBytesLeft       :: !BytesLeft,
    _announceRequestAnnounceEvent   :: !AnnounceEvent,
    _announceRequestIPAddress       :: !IPAddress,
    _announceRequestKey             :: !PeerKey,
    _announceRequestPeersWanted     :: !PeersWanted,
    _announceRequestPort            :: !PeerPort
} deriving (Show)


data ScrapeRequestInner = ScrapeRequestInner {
    _scrapeRequestConnectionID      :: !ConnectionID,
    _scrapeRequestTransactionID     :: !TransactionID,
    _scrapeRequestInfoHashes        :: ![InfoHash]
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

newtype IPAddress = IPAddress Word32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype PeersWanted = PeersWanted Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
