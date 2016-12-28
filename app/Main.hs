module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Socket

import Control.Concurrent (forkIO, killThread)
import Control.Exception.Lifted (bracket)
import Control.Monad (replicateM, forever, forM_)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.IO.Class (liftIO)

import Types
import Utils


main :: IO ()
main = do
    let config = Config {
        _serverAddress = "localhost:8080",
        _numberOfThreads = 4
    }

    initialState <- createInitialState config

    flip runReaderT initialState $ do
        runUDPServer

    return ()


createInitialState :: Config -> IO State
createInitialState config = State
    <$> return config
    <*> (STM.atomically $ STM.newTVar $ TorrentMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ ConnectionMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ [])


runUDPServer :: AppM ()
runUDPServer = bracket createSocket killThreadsUsingSocket $ \socket -> do
    numberOfThreads <- getConfigField _numberOfThreads

    createdThreadIds <- replicateM numberOfThreads $
        ask >>= liftIO . forkIO . runReaderT (acceptConnections socket)

    withThreadIds (++ createdThreadIds)


createSocket :: AppM Socket.Socket
createSocket = do
    serverAddress <- getConfigField _serverAddress

    addrInfos <- liftIO $ Socket.getAddrInfo
        (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
        Nothing
        (Just serverAddress)

    -- Select the first option with IPv4
    let serverAddr = head $ filter (\addr -> Socket.addrFamily addr == Socket.AF_INET) addrInfos

    liftIO $ do
        socket <- Socket.socket
            (Socket.addrFamily serverAddr)
            Socket.Datagram
            Socket.defaultProtocol

        Socket.bind socket $ Socket.addrAddress serverAddr

        return socket


killThreadsUsingSocket :: Socket.Socket -> AppM ()
killThreadsUsingSocket socket = do
    threadIds <- getThreadIds

    liftIO $ do
        forM_ threadIds killThread
        Socket.close socket

    return ()


acceptConnections :: Socket.Socket -> AppM ()
acceptConnections socket =
    forever $ do
        (requestBytes, remoteAddress) <- liftIO $ Socket.recvFrom socket 2048

        request       <- convertBytesToRequest requestBytes
        response      <- handleRequest request
        responseBytes <- convertResponseToBytes response

        liftIO $ Socket.sendTo socket responseBytes remoteAddress

    where
        convertBytesToRequest bytes = undefined
        handleRequest request = undefined
        convertResponseToBytes response = undefined


test :: AppM ()
test = do
    withTorrentMap $ Map.insert 1 [1..10]
    withConnectionMap $ Map.insert 1 1

    liftIO $ putStrLn "jawohl"
