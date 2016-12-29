{-# LANGUAGE DuplicateRecordFields #-}

-- Peer and related data types
module Types.Peer where

import Data.Word (Word32)

import Types.Common


data Peer = Peer {
    _id           :: !PeerID,
    _connectionID :: !ConnectionID,
    _ipAddress    :: !IPAddress,
    _port         :: !PeerPort,
    _status       :: !PeerStatus,
    _lastAnnounce :: !TimeStamp
} deriving (Show, Eq, Ord)

data PeerStatus
    = PeerSeeding
    | PeerLeeching
    | PeerStopped

    deriving (Show, Eq, Ord)
