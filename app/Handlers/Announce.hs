{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Handlers.Announce where

import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket
import qualified Control.Concurrent.STM as STM

import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)

import Types
import Utils

import Handlers.Common


handleAnnounceRequest :: AnnounceRequestInner -> Socket.SockAddr -> AppM Response
handleAnnounceRequest innerRequest remoteAddress = do
    let transactionID = _transactionID (innerRequest :: AnnounceRequestInner)

    case determineIPAddress innerRequest remoteAddress of
        Just address -> do
            (processedPeers, currentPeer) <- alterPeerAndGetPeers innerRequest address
            -- Return stats and all peers for this torrent from the TorrentMap
            -- filteredPeers <- filterPeers processedPeers currentPeer peersWanted

            peers <- return processedPeers

            return $ AnnounceResponse $ AnnounceResponseInner {
                _transactionID  = transactionID,
                _interval       = 600, -- TODO SETTINGS
                _leechers       = fromIntegral $ countLeechers peers,
                _seeders        = fromIntegral $ countSeeders peers,
                _peers          = peers
            }
        Nothing -> return $ ErrorResponse $ ErrorResponseInner
            transactionID
            "Your IP could not be determined"

    where
        determineIPAddress innerRequest remoteAddress =
            let ipAddress = _ipAddress (innerRequest :: AnnounceRequestInner)
            in if ipAddress /= 0
                then Just $ IPv4Address $ fromIntegral ipAddress
                else getIPAddress remoteAddress



alterPeerAndGetPeers innerRequest address = do
    let infoHash = _infoHash (innerRequest :: AnnounceRequestInner)

    timestamp <- liftIO $ getTimestamp

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
    :: TimeStamp
    -> IPvXAddress
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


createPeer :: TimeStamp -> IPvXAddress -> AnnounceRequestInner -> PeerStatus -> Peer
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


{--
-- Determine what peers should be sent to the current peer
filterPeers :: Sequence.Seq Peer -> Peer -> PeersWanted -> IO (Sequence.Seq Peer)
filterPeers processedPeers currentPeer peersWanted = do
    let peersWanted' = fromIntegral peersWanted
        peersToSend = if peersWanted' > 0 then peersWanted' else settingMaximumPeersToSend
        nonCurrentPeers = Sequence.filter (/= currentPeer) processedPeers
        leechers = Sequence.filter (\peer -> _peerStatus peer == PeerLeeching) nonCurrentPeers
        seeders = Sequence.filter (\peer -> _peerStatus peer == PeerSeeding) nonCurrentPeers
    if Sequence.length nonCurrentPeers <= peersWanted'
        then return nonCurrentPeers
        else Sequence.take peersToSend <$> case _peerStatus currentPeer of
            -- Leechers should be sent a pseudorandom mix of seeders and leechers
            PeerLeeching -> Sequence.fromList <$> (shuffleList $ toList $ mixSequences seeders leechers)

            -- Seeders should mainly be sent leechers. For stopped peers,
            -- it doesn't matter what is sent
            _ -> return $ leechers Sequence.>< seeders
-}
