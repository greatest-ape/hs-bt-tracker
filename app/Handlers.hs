{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (
    handleRequest
) where

import qualified Data.HashMap.Strict as Map
import qualified Network.Socket as Socket

import qualified Utils

import Types

import Handlers.Common
import Handlers.Announce
import Handlers.Connect
import Handlers.Scrape


handleRequest :: Request -> Socket.SockAddr -> AppM Response

-- Handle connect request
handleRequest (ConnectRequest requestInner) remoteAddress = do
    handleConnectRequest requestInner remoteAddress

-- Handle announce request
handleRequest (AnnounceRequest requestInner) remoteAddress = do
    let transactionID = _transactionID (requestInner :: AnnounceRequestInner)
        connectionID  = _connectionID  (requestInner :: AnnounceRequestInner)

    ifConnectionEstablished remoteAddress transactionID connectionID $ do
        handleAnnounceRequest requestInner remoteAddress

-- Handle scrape request
handleRequest (ScrapeRequest requestInner) remoteAddress = do
    let transactionID = _transactionID (requestInner :: ScrapeRequestInner)
        connectionID  = _connectionID  (requestInner :: ScrapeRequestInner)

    ifConnectionEstablished remoteAddress transactionID connectionID $ do
        handleScrapeRequest requestInner

-- Handle invalid request
handleRequest InvalidRequest _ = return $ ErrorResponse $ ErrorResponseInner
    (TransactionID 0)
    "Error: invalid request"


ifConnectionEstablished remoteAddress transactionID connectionID f = do
    connectionIsEstablished <- checkConnectionEstablished connectionID remoteAddress

    if connectionIsEstablished
        then f
        else do
            return $ ErrorResponse $ ErrorResponseInner
                transactionID
                "Error: no connection is established"


-- Check if entry for combination of connectionID and ipAddress exists
checkConnectionEstablished :: ConnectionID -> Socket.SockAddr -> AppM Bool
checkConnectionEstablished connectionID remoteAddress = do
    case getIPAddress remoteAddress of
        Just address -> Utils.getsConnectionMap $
            Map.member (ConnectionMapKey connectionID address)
        Nothing -> return False
