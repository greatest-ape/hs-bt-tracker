{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.BitTorrent.Tracker.Handlers.Connect (
    handleConnectRequest
) where

import qualified Data.HashMap.Strict as Map
import qualified Network.Socket as Socket

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import System.Random (randomIO)

import qualified Web.BitTorrent.Tracker.Utils as Utils

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Handlers.Common


handleConnectRequest :: ConnectRequestInner -> Socket.SockAddr -> AppM Response
handleConnectRequest innerRequest remoteAddress = do
    let transactionID = _transactionID (innerRequest :: ConnectRequestInner)

    case getIPAddress remoteAddress of
        Just address -> do
            connectionID <- ConnectionID <$> (liftIO randomIO :: AppM Int64)
            timestamp    <- liftIO $ Utils.getTimestamp
            Utils.withConnectionMap $ Map.insert (ConnectionMapKey connectionID address) timestamp

            return $ ConnectResponse $ ConnectResponseInner connectionID transactionID
        Nothing ->
            return $ ErrorResponse $ ErrorResponseInner transactionID "Invalid IP"
