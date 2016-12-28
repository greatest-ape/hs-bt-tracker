-- Peer and related data types
module Types.Peer where

import Data.Word (Word32)

import Types.Common


data Peer = Peer {
    _peerID           :: !PeerID,
    _peerConnectionID :: !ConnectionID,
    _peerIPAddress    :: !IPvXAddress,
    _peerPort         :: !PeerPort,
    _peerStatus       :: !PeerStatus,
    _peerLastAnnounce :: !TimeStamp
} deriving (Show, Eq, Ord)

data PeerStatus
    = PeerSeeding
    | PeerLeeching
    | PeerStopped

    deriving (Show, Eq, Ord)

data IPvXAddress
    = IPv4Address !Word32
    | IPv6Address !(Word32, Word32, Word32, Word32)

    deriving (Show, Eq, Ord)


