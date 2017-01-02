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

    deriving (Show, Eq)


data ConnectResponseInner = ConnectResponseInner {
    _connectionID    :: !ConnectionID,
    _transactionID   :: !TransactionID
} deriving (Show, Eq)


data AnnounceResponseInner = AnnounceResponseInner {
    _transactionID  :: !TransactionID,
    _interval       :: !AnnounceInterval,
    _leechers       :: !NumberOfLeechers,
    _seeders        :: !NumberOfSeeders,
    _peers          :: !(Sequence.Seq Peer)
} deriving (Show, Eq)


data ScrapeResponseInner = ScrapeResponseInner {
    _transactionID        :: !TransactionID,
    _torrentStatistics    :: !(Sequence.Seq TorrentScrapeStatistics)
} deriving (Show, Eq)


data ErrorResponseInner = ErrorResponseInner {
    _transactionID     :: !TransactionID,
    _message           :: !BS.ByteString
} deriving (Show, Eq)


newtype AnnounceInterval = AnnounceInterval Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

data TorrentScrapeStatistics = TorrentScrapeStatistics {
    _seeders     :: !NumberOfSeeders,
    _completed   :: !NumberOfDownloads,
    _leechers    :: !NumberOfLeechers
} deriving (Show, Eq)

newtype NumberOfSeeders = NumberOfSeeders Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfLeechers = NumberOfLeechers Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfDownloads = NumberOfDownloads Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

