{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.BitTorrent.Tracker.Handlers (
    handleRequest
) where

import qualified Data.HashMap.Strict as Map
import qualified Network.Socket as Socket

import qualified Web.BitTorrent.Tracker.Utils as Utils

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Handlers.Common
import Web.BitTorrent.Tracker.Handlers.Announce
import Web.BitTorrent.Tracker.Handlers.Connect
import Web.BitTorrent.Tracker.Handlers.Scrape


handleRequest :: Request -> Socket.SockAddr -> AppM Response

-- Handle connect request
handleRequest (ConnectRequest innerRequest) remoteAddress =
    handleConnectRequest innerRequest remoteAddress

-- Handle announce request
handleRequest (AnnounceRequest innerRequest) remoteAddress = do
    let transactionID = _transactionID (innerRequest :: AnnounceRequestInner)
        connectionID  = _connectionID  (innerRequest :: AnnounceRequestInner)

    ifConnectionEstablished remoteAddress transactionID connectionID $
        handleAnnounceRequest innerRequest remoteAddress

-- Handle scrape request
handleRequest (ScrapeRequest innerRequest) remoteAddress = do
    let transactionID = _transactionID (innerRequest :: ScrapeRequestInner)
        connectionID  = _connectionID  (innerRequest :: ScrapeRequestInner)

    ifConnectionEstablished remoteAddress transactionID connectionID $
        handleScrapeRequest innerRequest

-- Handle invalid request
handleRequest InvalidRequest _ = return $ ErrorResponse $ ErrorResponseInner
    (TransactionID 0)
    "Error: invalid request"


ifConnectionEstablished
    :: Socket.SockAddr
    -> TransactionID
    -> ConnectionID
    -> AppM Response
    -> AppM Response
ifConnectionEstablished remoteAddress transactionID connectionID f = do
    connectionIsEstablished <- checkConnectionEstablished connectionID remoteAddress

    if connectionIsEstablished
        then f
        else
            return $ ErrorResponse $ ErrorResponseInner
                transactionID
                "Error: no connection is established"


-- Check if entry for combination of connectionID and ipAddress exists
checkConnectionEstablished :: ConnectionID -> Socket.SockAddr -> AppM Bool
checkConnectionEstablished connectionID remoteAddress =
    case getIPAddress remoteAddress of
        Just address -> Utils.getsConnectionMap $
            Map.member (ConnectionMapKey connectionID address)
        Nothing -> return False
