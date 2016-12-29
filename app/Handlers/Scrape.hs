{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Scrape (
    handleScrapeRequest
) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence

import Types
import Utils

import Handlers.Common


handleScrapeRequest :: ScrapeRequestInner -> AppM Response
handleScrapeRequest innerRequest = do
    let transactionID = _transactionID (innerRequest :: ScrapeRequestInner)
    let infoHashes    = _infoHashes    (innerRequest :: ScrapeRequestInner)

    peerLists <- getPeerLists infoHashes

    return $ ScrapeResponse $ ScrapeResponseInner {
        _transactionID     = transactionID,
        _torrentStatistics = fmap buildStats peerLists
    }


getPeerLists :: Sequence.Seq InfoHash -> AppM (Sequence.Seq (Sequence.Seq Peer))
getPeerLists infoHashes = do
    tm <- getTorrentMap
    return $ foldr (f tm) Sequence.empty infoHashes

    where
        f tm infoHash xs =
            case Map.lookup infoHash tm of
                Just peers -> peers Sequence.<| xs
                Nothing -> xs


buildStats :: Sequence.Seq Peer -> TorrentScrapeStatistics
buildStats peers = TorrentScrapeStatistics {
    _seeders     = NumberOfSeeders $ fromIntegral $ countSeeders peers,
    _completed   = NumberOfDownloads 0, -- Not implemented
    _leechers    = NumberOfLeechers $ fromIntegral $ countLeechers peers
}
