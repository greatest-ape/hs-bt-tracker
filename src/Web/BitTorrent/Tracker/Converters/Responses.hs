{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.BitTorrent.Tracker.Converters.Responses where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Monad (forM_)
import Data.Word (Word32)

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Converters.Common


responseToBytes :: Response -> BS.ByteString
responseToBytes response = LBS.toStrict $ Binary.runPut $ case response of
    ConnectResponse responseInner -> do
        encodeActionAndTransactionID 0   $ _transactionID (responseInner :: ConnectResponseInner)
        Binary.putInt64be $ fromIntegral $ _connectionID (responseInner :: ConnectResponseInner)

    AnnounceResponse responseInner -> do
        encodeActionAndTransactionID 1 $ _transactionID (responseInner :: AnnounceResponseInner)
        Binary.putInt32be $ fromIntegral $ _interval (responseInner :: AnnounceResponseInner)
        Binary.putInt32be $ fromIntegral $ _leechers (responseInner :: AnnounceResponseInner)
        Binary.putInt32be $ fromIntegral $ _seeders (responseInner :: AnnounceResponseInner)

        forM_ (_peers (responseInner :: AnnounceResponseInner)) $ \peer -> do
            Binary.putWord32be $ fromIntegral $ _ipAddress (peer :: Peer)
            Binary.putWord16be $ fromIntegral $ _port      (peer :: Peer)

    ScrapeResponse responseInner -> do
        encodeActionAndTransactionID 2 $ _transactionID (responseInner :: ScrapeResponseInner)

        forM_ (_torrentStatistics (responseInner :: ScrapeResponseInner)) $ \torrentStatistics -> do
            Binary.putInt32be $ fromIntegral $ _seeders (torrentStatistics :: TorrentScrapeStatistics)
            Binary.putInt32be $ fromIntegral $ _completed (torrentStatistics :: TorrentScrapeStatistics)
            Binary.putInt32be $ fromIntegral $ _leechers (torrentStatistics :: TorrentScrapeStatistics)

    ErrorResponse responseInner -> do
        encodeActionAndTransactionID 3 $ _transactionID (responseInner :: ErrorResponseInner)
        Binary.putByteString $ _message responseInner

    NoResponse -> return ()

    where
        encodeActionAndTransactionID :: Word32 -> TransactionID -> Binary.PutM ()
        encodeActionAndTransactionID action tid = do
            Binary.putInt32be $ fromIntegral action
            Binary.putInt32be $ fromIntegral tid


bytesToResponse byteString = (flip Binary.runGetOrFail) (LBS.fromStrict byteString) $ do
    action <- Binary.getWord32be
    transactionID <- TransactionID . fromIntegral <$> Binary.getWord32be

    case action of
        0 -> do -- Connect
            connectionID <- ConnectionID . fromIntegral <$> Binary.getWord64be
            return $ ConnectResponse $ ConnectResponseInner connectionID transactionID
        1 -> do -- Announce
            interval <- AnnounceInterval . fromIntegral <$> Binary.getWord32be
            leechers <- NumberOfLeechers . fromIntegral <$> Binary.getWord32be
            seeders <- NumberOfSeeders . fromIntegral <$> Binary.getWord32be
            peers <- binaryGetMany $ do
                ipAddress <- IPAddress <$> Binary.getWord32be
                port <- PeerPort <$> Binary.getWord16be

                return Peer {
                    _id = PeerID "",
                    _connectionID = ConnectionID 0,
                    _ipAddress = ipAddress,
                    _port = port,
                    _status = PeerLeeching,
                    _lastAnnounce = Timestamp 1
                }

            return $ AnnounceResponse $ AnnounceResponseInner {
                _transactionID  = transactionID,
                _interval       = interval,
                _leechers       = leechers,
                _seeders        = seeders,
                _peers          = peers
            }
        2 -> do -- Scrape
            statistics <- binaryGetMany $ do
                seeders <- fromIntegral <$> Binary.getInt32be
                completed <- fromIntegral <$> Binary.getInt32be
                leechers <- fromIntegral <$> Binary.getInt32be

                return TorrentScrapeStatistics {
                    _seeders     = seeders,
                    _completed   = completed,
                    _leechers    = leechers
                }

            return $ ScrapeResponse $ ScrapeResponseInner {
                _transactionID        = transactionID,
                _torrentStatistics    = statistics

            }
        3 -> do -- Error
            errorMessage <- LBS.toStrict <$> Binary.getRemainingLazyByteString

            return $ ErrorResponse $ ErrorResponseInner transactionID errorMessage
