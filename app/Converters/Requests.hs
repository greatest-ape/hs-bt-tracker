{-# LANGUAGE DuplicateRecordFields #-}

module Converters.Requests where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Types

import Converters.Common


requestToBytes :: Request -> BS.ByteString
requestToBytes request = LBS.toStrict $ Binary.runPut $ case request of
    ConnectRequest requestInner -> do
        Binary.putInt64be  0x41727101980
        Binary.putInt32be  0 -- Action: connect
        Binary.putInt32be  $ fromIntegral $ _transactionID   (requestInner :: ConnectRequestInner)

    AnnounceRequest requestInner -> do
        Binary.putInt64be  $ fromIntegral $ _connectionID    (requestInner :: AnnounceRequestInner)
        Binary.putInt32be  1 -- Action: announce
        Binary.putInt32be  $ fromIntegral $ _transactionID   (requestInner :: AnnounceRequestInner)

        let (InfoHash infoHashByteString) = _infoHash        (requestInner :: AnnounceRequestInner)
        Binary.putByteString infoHashByteString

        let (PeerID peerIDByteString)     = _peerID          (requestInner :: AnnounceRequestInner)
        Binary.putByteString peerIDByteString

        Binary.putInt64be  $ fromIntegral $ _bytesDownloaded (requestInner :: AnnounceRequestInner)
        Binary.putInt64be  $ fromIntegral $ _bytesLeft       (requestInner :: AnnounceRequestInner)
        Binary.putInt64be  $ fromIntegral $ _bytesUploaded   (requestInner :: AnnounceRequestInner)
        Binary.putInt32be  $ encodeEvent  $ _announceEvent   (requestInner :: AnnounceRequestInner)
        Binary.putWord32be $ fromIntegral $ _ipAddress       (requestInner :: AnnounceRequestInner)
        Binary.putWord32be $ fromIntegral $ _key             (requestInner :: AnnounceRequestInner)
        Binary.putInt32be  $ fromIntegral $ _peersWanted     (requestInner :: AnnounceRequestInner)
        Binary.putWord16be $ fromIntegral $ _port            (requestInner :: AnnounceRequestInner)

    ScrapeRequest requestInner -> do
        Binary.putInt64be  $ fromIntegral $ _connectionID    (requestInner :: ScrapeRequestInner)
        Binary.putInt32be  2 -- Action: scrape
        Binary.putInt32be  $ fromIntegral $ _transactionID   (requestInner :: ScrapeRequestInner)

        mapM (\(InfoHash b) -> Binary.putByteString b) (_infoHashes (requestInner :: ScrapeRequestInner))
        return ()

    InvalidRequest -> return ()

    where
        encodeEvent AnnounceEventCompleted = 1
        encodeEvent AnnounceEventStarted = 2
        encodeEvent AnnounceEventStopped = 3
        encodeEvent AnnounceEventNone = 0


bytesToRequest
    :: BS.ByteString -- ^ Request as ByteString
    -> Either
        (LBS.ByteString, Binary.ByteOffset, String) -- Error message
        (LBS.ByteString, Binary.ByteOffset, Request) -- Decoded request
bytesToRequest byteString = (flip Binary.runGetOrFail) (LBS.fromStrict byteString) $ do
    connectionID     <- ConnectionID . fromIntegral <$> Binary.getInt64be
    action           <- fromIntegral <$> Binary.getInt32be
    transactionID    <- TransactionID . fromIntegral <$> Binary.getInt32be

    case action of
        -- Connect request
        0 -> if connectionID == ConnectionID 0x41727101980 -- Protocol identifier
            then return $ ConnectRequest $ ConnectRequestInner transactionID
            else return InvalidRequest

        -- Announce request
        1 -> do
            infoHash        <- InfoHash <$> Binary.getByteString 20
            peerID          <- PeerID <$> Binary.getByteString 20
            bytesDownloaded <- BytesDownloaded . fromIntegral <$> Binary.getInt64be
            bytesLeft       <- BytesLeft . fromIntegral <$> Binary.getInt64be
            bytesUploaded   <- BytesUploaded . fromIntegral <$> Binary.getInt64be
            event           <- decodeEvent <$> Binary.getInt32be
            ipAddress       <- IPAddress <$> Binary.getWord32be
            key             <- PeerKey . fromIntegral <$> Binary.getWord32be
            peersWanted     <- PeersWanted . fromIntegral <$> Binary.getInt32be
            port            <- PeerPort . fromIntegral <$> Binary.getWord16be

            return $ AnnounceRequest $ AnnounceRequestInner {
                _connectionID    = connectionID,
                _transactionID   = transactionID,
                _infoHash        = infoHash,
                _peerID          = peerID,
                _bytesDownloaded = bytesDownloaded,
                _bytesUploaded   = bytesUploaded,
                _bytesLeft       = bytesLeft,
                _announceEvent   = event,
                _ipAddress       = ipAddress,
                _key             = key,
                _peersWanted     = peersWanted,
                _port            = port
            }

        -- Scrape request
        2 -> do
            infoHashes <- fmap (fmap InfoHash) $ binaryGetMany $ Binary.getByteString 20

            return $ ScrapeRequest $ ScrapeRequestInner {
                _connectionID  = connectionID,
                _transactionID = transactionID,
                _infoHashes    = infoHashes
            }

        n -> fail $ "Invalid action: " ++ show n

    where
        decodeEvent 1 = AnnounceEventCompleted
        decodeEvent 2 = AnnounceEventStarted
        decodeEvent 3 = AnnounceEventStopped
        decodeEvent _ = AnnounceEventNone
