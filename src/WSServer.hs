-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module WSServer (
    main
) where

import qualified Control.Exception      as CE
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.UTF8   as BU
import qualified Data.ByteString.Lazy   as BL
import qualified Data.TCache            as TC
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Network.WebSockets     as NW


import qualified WSCommon               as WSC
import qualified WSAction               as WSA

--------------------------------------------------------------------------------

data ConnId = ConnId String               deriving (Eq, Ord, Show)
data Conn  = Conn ConnId NW.Connection
type Pool    = [Conn]
type STMPool = STM.TVar Pool

--------------------------------------------------------------------------------
--Example action and middleware-------------------------------------------------
action :: WSC.Message -> IO WSC.Message
action message@(WSC.Message Nothing control) = return message
action message@(WSC.Message (Just txt) control) = do
    putStrLn (show message)
    case txt of
        "13" -> return (WSC.Message (Just txt) (Just False))
        _    -> return message

middleware :: WSC.Message -> (WSC.Message -> IO WSC.Message) -> IO WSC.Message
middleware msg action = putStrLn (show msg) >> action msg >>= return


--------------------------------------------------------------------------------


receiveData :: Conn -> IO WSC.Message
receiveData (Conn cid connection) = do
    msg <- NW.receiveData connection
    return (WSC.Message (Just msg) Nothing)


sendData :: Conn -> WSC.Message -> IO WSC.Message
sendData conn message@(WSC.Message Nothing control) = return message
sendData (Conn id connection) message@(WSC.Message (Just msg) control) = do
    NW.sendTextData connection msg
    return message


broadcast_ :: Pool -> Conn -> WSC.Message -> IO WSC.Message
broadcast_ [] conn message = return message
broadcast_ (x@(Conn xcid _):xs) conn@(Conn cid _) message = do
    sendData x message
    putStrLn $ (show message)++" from "++(show cid)++" to "++(show xcid)
    broadcast_ xs conn message


broadcastData :: STMPool -> Conn -> WSC.Message -> IO WSC.Message
broadcastData pool conn@(Conn cid _) message = do
    connections <- STM.atomically (STM.readTVar pool)
    broadcast_ (filter (\(Conn x _) -> (x /= cid)) connections) conn message


--------------------------------------------------------------------------------


respond :: STMPool -> Conn -> (WSC.Message -> IO WSC.Message) -> IO WSC.Message
respond pool conn proc = 
    receiveData conn >>= proc >>= broadcastData pool conn

mainloop :: STMPool -> Conn -> IO ()
mainloop pool conn = do
    message <- respond pool conn WSA.processChangeset
    case message of
        (WSC.Message _ (Just False)) -> return ()
        _                            -> mainloop pool conn


talk :: STMPool -> Conn -> IO ()
talk pool conn@(Conn cid connection) = do
    CE.handle catchDisconnect (mainloop pool conn)
    where
        catchDisconnect e = do 
            STM.atomically $ removeConnection pool cid
            let exc = CE.fromException e
            case exc of
                Just NW.ConnectionClosed -> do
                    putStrLn $ "Connection closed: " ++ (show cid)
                _                        -> do
                    putStrLn $ "Exception: "++(show exc)++" CID:"++(show cid)

--------------------------------------------------------------------------------

addConnection :: STMPool -> Conn -> STM.STM ()
addConnection pool conn = 
    STM.orElse (
        do
            _pool <- STM.readTVar pool
            STM.writeTVar pool (conn:_pool)
            TC.safeIOToSTM $ putStrLn $ "After add: " ++ (show $ length (conn:_pool))
            
    ) (
            TC.safeIOToSTM $ putStrLn $ "Add conn: Transaction cancelled"
    )

removeConnection :: STMPool -> ConnId -> STM.STM ()
removeConnection pool cid = 
    STM.orElse (
        do
            p <- STM.readTVar pool
            STM.writeTVar pool (filter (\(Conn _id _conn) -> (_id /= cid)) p) 
            TC.safeIOToSTM $ putStrLn $ "After removal: " ++ (show $ length p)
    ) (
            TC.safeIOToSTM $ putStrLn $ "Remove conn: Transaction cancelled"
    )

--------------------------------------------------------------------------------


application :: STMPool -> NW.PendingConnection -> IO ()
application pool pendingConnection = do
    let requestHeaders = NW.requestHeaders $ NW.pendingRequest pendingConnection
    let clientIP = lookup "X-Real-IP" requestHeaders
    let cid   = lookup "Sec-WebSocket-Key" requestHeaders
    case cid of
        Nothing -> do 
            let errmsg = "ERROR: NO Sec-WebSocket-Key provided."
            NW.rejectRequest pendingConnection errmsg
        Just cid -> do
            putStrLn $ "New connection: "++(show clientIP)++" CID: "++(show cid)
            --putStrLn $ show requestHeaders
            let _cid = BU.toString cid
            connection <- NW.acceptRequest pendingConnection 
            STM.atomically $ addConnection pool (Conn (ConnId _cid) connection)
            talk pool (Conn (ConnId _cid) connection)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    pool <- STM.atomically $ STM.newTVar []
    NW.runServer "127.0.0.1" 1313 (application pool)


















                
