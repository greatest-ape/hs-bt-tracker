-- Peer and related data types
module Types.Peer where

import Types.Common


data Peer = Peer {
    _id           :: !PeerID,
    _connectionID :: !ConnectionID,
    _ipAddress    :: !IPAddress,
    _port         :: !PeerPort,
    _status       :: !PeerStatus,
    _lastAnnounce :: !Timestamp
} deriving (Show, Eq, Ord)

data PeerStatus
    = PeerSeeding
    | PeerLeeching
    | PeerStopped

    deriving (Show, Eq, Ord)
