{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Response and related data types
module Types.Response where

import Data.Int (Int32)

import qualified Data.ByteString.Char8 as BS

import Types.Common
import Types.Peer


data ResponseWrapper
    = ConnectResponse ConnectResponseInner
    | AnnounceResponse AnnounceResponseInner
    | ScrapeResponse ScrapeResponseInner
    | ErrorResponse ErrorResponseInner
    | NoResponse


data ConnectResponseInner = ConnectResponseInner {
    _connectResponseConnectionID    :: !ConnectionID,
    _connectResponseTransactionID   :: !TransactionID
} deriving (Show)


data AnnounceResponseInner = AnnounceResponseInner {
    _announceResponseTransactionID  :: !TransactionID,
    _announceResponseInterval       :: !AnnounceInterval,
    _announceResponseLeechers       :: !NumberOfLeechers,
    _announceResponseSeeders        :: !NumberOfSeeders,
    _announceResponsePeers          :: ![Peer]
} deriving (Show)


data ScrapeResponseInner = ScrapeResponseInner {
    _scrapeResponseTransactionID        :: !TransactionID,
    _scrapeResponseTorrentStatistics    :: ![TorrentScrapeStatistics]
} deriving (Show)


data ErrorResponseInner = ErrorResponseInner {
    _errorResponseTransactionID     :: !TransactionID,
    _errorResponseMessage           :: !BS.ByteString
} deriving (Show)


newtype AnnounceInterval = AnnounceInterval Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

data TorrentScrapeStatistics = TorrentScrapeStatistics {
    _torrentScrapeStatisticsSeeders     :: NumberOfSeeders,
    _torrentScrapeStatisticsCompleted   :: NumberOfDownloads,
    _torrentScrapeStatisticsLeechers    :: NumberOfLeechers
} deriving (Show)

newtype NumberOfSeeders = NumberOfSeeders Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfLeechers = NumberOfLeechers Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype NumberOfDownloads = NumberOfDownloads Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

