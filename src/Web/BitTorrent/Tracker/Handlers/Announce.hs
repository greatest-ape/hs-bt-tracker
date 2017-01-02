{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Web.BitTorrent.Tracker.Handlers.Announce where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Array.IO (readArray, writeArray, newListArray, IOArray)
import Data.Foldable (toList)
import System.Random (randomIO, randomRIO)

import qualified Web.BitTorrent.Tracker.Utils as Utils

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Handlers.Common


handleAnnounceRequest
    :: AnnounceRequestInner
    -> Socket.SockAddr
    -> AppM Response
handleAnnounceRequest innerRequest remoteAddress = do
    let transactionID = _transactionID (innerRequest :: AnnounceRequestInner)
        peersWanted   = _peersWanted   (innerRequest :: AnnounceRequestInner)

    case determineIPAddress innerRequest remoteAddress of
        Just address -> do
            (processedPeers, currentPeer) <- alterPeerAndGetPeers innerRequest address
            filteredPeers <- filterPeers processedPeers currentPeer peersWanted

            announceInterval <- Utils.getConfigField _announceInterval

            return $ AnnounceResponse $ AnnounceResponseInner {
                _transactionID  = transactionID,
                _interval       = AnnounceInterval announceInterval,
                _leechers       = countLeechers filteredPeers,
                _seeders        = countSeeders filteredPeers,
                _peers          = filteredPeers
            }
        Nothing -> return $ ErrorResponse $ ErrorResponseInner
            transactionID
            "Your IP could not be determined"

    where
        determineIPAddress :: AnnounceRequestInner -> Socket.SockAddr -> Maybe IPAddress
        determineIPAddress innerRequest remoteAddress =
            let ipAddress = _ipAddress (innerRequest :: AnnounceRequestInner)
            in if ipAddress /= 0
                then Just $ fromIntegral ipAddress
                else getIPAddress remoteAddress



alterPeerAndGetPeers
    :: AnnounceRequestInner
    -> IPAddress
    -> AppM (Sequence.Seq Peer, Peer)
alterPeerAndGetPeers innerRequest address = do
    let infoHash = _infoHash (innerRequest :: AnnounceRequestInner)

    timestamp <- liftIO $ Utils.getTimestamp

    tVar <- asks _torrentMap

    liftIO $ STM.atomically $ do
        (TorrentMap tm) <- STM.readTVar tVar

        let maybePeers = Map.lookup infoHash tm
        let (processedPeers, currentPeer) = alterPeerSequence timestamp address innerRequest maybePeers

        STM.modifyTVar tVar $ \(TorrentMap tm) -> (TorrentMap $ Map.insert infoHash processedPeers tm)

        return (processedPeers, currentPeer)


-- Update a peer in a peer list, or if it is not currently present, insert it
-- Returns the list and the peer
alterPeerSequence
    :: Timestamp
    -> IPAddress
    -> AnnounceRequestInner
    -> Maybe (Sequence.Seq Peer)
    -> (Sequence.Seq Peer, Peer)
alterPeerSequence timestamp ipAddress innerRequest maybePeers =
    let connectionID  = _connectionID  (innerRequest :: AnnounceRequestInner)
        peerID        = _peerID        (innerRequest :: AnnounceRequestInner)
        announceEvent = _announceEvent (innerRequest :: AnnounceRequestInner)
        bytesLeft     = _bytesLeft     (innerRequest :: AnnounceRequestInner)

        newPeer = createPeer timestamp ipAddress innerRequest PeerLeeching

    in case maybePeers of
        -- This is the first peer for this torrent, return a singleton sequence
        Nothing -> (Sequence.singleton newPeer, newPeer)

        -- This torrent has a list of peers, but the list is empty. Return a singleton sequence
        (Just (Sequence.viewl -> Sequence.EmptyL)) -> (Sequence.singleton newPeer, newPeer)

        -- This torrent already has peers attached
        (Just peers) ->
            let matchingPeers    = Sequence.filter (      identifyPeer ipAddress innerRequest) peers
                nonMatchingPeers = Sequence.filter (not . identifyPeer ipAddress innerRequest) peers

            in case matchingPeers of
                -- Peer is not in list, add it
                (Sequence.viewl -> Sequence.EmptyL) -> (newPeer Sequence.<| nonMatchingPeers, newPeer)

                -- Peer is in list, add it to the list of non-matching peers and return that
                (Sequence.viewl -> matchingPeer Sequence.:< _) ->
                    let peer = createPeer timestamp ipAddress innerRequest (_status (matchingPeer :: Peer))
                    in (peer Sequence.<| nonMatchingPeers, peer)

    where
        identifyPeer ipAddress innerRequest peer =
            let connectionID = _connectionID  (innerRequest :: AnnounceRequestInner)
                peerID       = _peerID        (innerRequest :: AnnounceRequestInner)

            in
                _ipAddress (peer :: Peer) == ipAddress &&
                (
                    _connectionID (peer :: Peer) == connectionID ||
                    _id           (peer :: Peer) == peerID
                )


createPeer
    :: Timestamp
    -> IPAddress
    -> AnnounceRequestInner
    -> PeerStatus
    -> Peer
createPeer timestamp ipAddress innerRequest defaultStatus =
    let AnnounceRequestInner {..} = innerRequest in Peer {
        _id             = _peerID,
        _connectionID   = _connectionID,
        _ipAddress      = ipAddress,
        _port           = _port,
        _status         = getPeerStatus _announceEvent _bytesLeft defaultStatus,
        _lastAnnounce   = timestamp
    }


-- Make a reasonable guess about the status of a peer
getPeerStatus :: AnnounceEvent -> BytesLeft -> PeerStatus -> PeerStatus
getPeerStatus announceEvent bytesLeft defaultStatus
    | announceEvent == AnnounceEventStopped     = PeerStopped
    | bytesLeft     == BytesLeft 0              = PeerSeeding
    | announceEvent == AnnounceEventStarted     = PeerLeeching
    | announceEvent == AnnounceEventCompleted   = PeerSeeding
    | otherwise                                 = defaultStatus



-- Determine what peers should be sent to the current peer
filterPeers
    :: Sequence.Seq Peer
    -> Peer
    -> PeersWanted
    -> AppM (Sequence.Seq Peer)
filterPeers processedPeers currentPeer peersWanted = do
    maximumPeersToSend <- Utils.getConfigField _maximumPeersToSend

    let peersWanted' = fromIntegral peersWanted
        peersToSend =
            if peersWanted' < maximumPeersToSend && peersWanted' > 0
                then peersWanted'
                else maximumPeersToSend

        nonCurrentPeers = Sequence.filter (/= currentPeer) processedPeers
        leechers = Sequence.filter (\peer -> _status (peer :: Peer) == PeerLeeching) nonCurrentPeers
        seeders  = Sequence.filter (\peer -> _status (peer :: Peer) == PeerSeeding) nonCurrentPeers

    if Sequence.length nonCurrentPeers <= peersWanted'
        -- If less peers exist than the number supposed to be sent, just send
        -- all of them (no selection, mixing etc)
        then return nonCurrentPeers

        -- Otherwise, send a specific selection bases on the status of the
        -- requesting peer
        else Sequence.take peersToSend <$> case _status (currentPeer :: Peer) of
            -- Leechers should be sent a pseudorandom mix of seeders and leechers
            PeerLeeching -> liftIO $ shuffleSequence $ mixSequences seeders leechers

            -- Seeders should mainly be sent leechers. For stopped peers,
            -- it doesn't matter what is sent
            _ -> return $ leechers Sequence.>< seeders


-- Mix elements from two sequences
mixSequences :: Sequence.Seq a -> Sequence.Seq a -> Sequence.Seq a
mixSequences (Sequence.viewl -> x Sequence.:< xs) (Sequence.viewl -> y Sequence.:< ys) =
     x Sequence.<| y Sequence.<| mixSequences xs ys
mixSequences (Sequence.viewl -> Sequence.EmptyL) ys = ys
mixSequences xs (Sequence.viewl -> Sequence.EmptyL) = xs


-- | Randomly shuffle a sequence
--   /O(N)/
--   inspired by https://wiki.haskell.org/Random_shuffle
--   Needs to be tested (for Seq monad semantics)
shuffleSequence :: Sequence.Seq a -> IO (Sequence.Seq a)
shuffleSequence xs = do
    let n = length xs

    ar <- newArray n $ toList xs
    forM (Sequence.fromList [1..n]) $ \i -> do
        j  <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

    where
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs
