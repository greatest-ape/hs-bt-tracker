{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.BitTorrent.Tracker.Handlers.Scrape (
    handleScrapeRequest
) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence

import qualified Web.BitTorrent.Tracker.Utils as Utils

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Handlers.Common


handleScrapeRequest :: ScrapeRequestInner -> AppM Response
handleScrapeRequest innerRequest = do
    let transactionID = _transactionID (innerRequest :: ScrapeRequestInner)
        infoHashes    = _infoHashes    (innerRequest :: ScrapeRequestInner)

    peerLists <- getPeerLists infoHashes

    return $ ScrapeResponse $ ScrapeResponseInner {
        _transactionID     = transactionID,
        _torrentStatistics = fmap buildStats peerLists
    }


getPeerLists :: Sequence.Seq InfoHash -> AppM (Sequence.Seq (Sequence.Seq Peer))
getPeerLists infoHashes = do
    tm <- Utils.getTorrentMap
    return $ foldr (f tm) Sequence.empty infoHashes

    where
        f tm infoHash xs =
            case Map.lookup infoHash tm of
                Just peers -> peers Sequence.<| xs
                Nothing -> xs


buildStats :: Sequence.Seq Peer -> TorrentScrapeStatistics
buildStats peers = TorrentScrapeStatistics {
    _seeders     = countSeeders peers,
    _completed   = NumberOfDownloads 0, -- Not implemented
    _leechers    = countLeechers peers
}
