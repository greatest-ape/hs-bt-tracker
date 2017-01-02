{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- Response and related data types
module Web.BitTorrent.Tracker.Types.Response where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Sequence

import Data.Int (Int32)

import Web.BitTorrent.Tracker.Types.Common
import Web.BitTorrent.Tracker.Types.Peer


data Response
    = ConnectResponse ConnectResponseInner
    | AnnounceResponse AnnounceResponseInner
    | ScrapeResponse ScrapeResponseInner
    | ErrorResponse ErrorResponseInner
    | NoResponse


data ConnectResponseInner = ConnectResponseInner {
    _connectionID    :: !ConnectionID,
    _transactionID   :: !TransactionID
} deriving (Show)


data AnnounceResponseInner = AnnounceResponseInner {
    _transactionID  :: !TransactionID,
    _interval       :: !AnnounceInterval,
    _leechers       :: !NumberOfLeechers,
    _seeders        :: !NumberOfSeeders,
    _peers          :: !(Sequence.Seq Peer)
} deriving (Show)


data ScrapeResponseInner = ScrapeResponseInner {
    _transactionID        :: !TransactionID,
    _torrentStatistics    :: !(Sequence.Seq TorrentScrapeStatistics)
} deriving (Show)


data ErrorResponseInner = ErrorResponseInner {
    _transactionID     :: !TransactionID,
    _message           :: !BS.ByteString
} deriving (Show)


newtype AnnounceInterval = AnnounceInterval Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

data TorrentScrapeStatistics = TorrentScrapeStatistics {
    _seeders     :: !NumberOfSeeders,
    _completed   :: !NumberOfDownloads,
    _leechers    :: !NumberOfLeechers
} deriving (Show)

newtype NumberOfSeeders = NumberOfSeeders Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfLeechers = NumberOfLeechers Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfDownloads = NumberOfDownloads Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

