module Network.Peerstore (
    Peerstore(..)
) where

import Control.Concurrent.STM
import LibP2P.Protocol (Protocol)
import Data.HashMap.Lazy (empty)

-- TODO : Protocol should be extended later to a custom data type 
-- later that represents a fully validated protocol name in the peerstore

-- Peer related information for a given peerId. Supports concurrent access,
-- updates through STM Library.
data PeerInfo = PeerInfo
    {
        id :: PeerId
        addrs :: TVar [Multiaddr]
        protocols :: TVar [Protocol]
    }

-- Hashmap of peerinfo indexed by peer id. Concurrency through STM
data Peerstore = Peerstore (TVar (HashMap PeerId PeerInfo)) 

-- Return a new peerstore
newPeerstore :: STM Peerstore
newPeerstore = Peerstore (newTVar empty)

-- Return a list of stored peer ids
peers :: Peerstore -> STM [PeerId]
peers (Peerstore s) = do
    store <- readTVar s
    return $ keys store

-- Return peer related information for a given peer id 
peerInfo :: PeerId -> Peerstore -> STM (Maybe PeerInfo)
peerInfo pid (Peerstore s) = do
    store <- readTVar s
    return $ lookup pid store

-- Return a list of protocols that are supported by the given peer
getProtocols :: PeerId -> Peerstore -> STM (Maybe [Protocol])
getProtocols pid ps@(Peerstore s) = do
    pi <- peerInfo pid ps
    return $ readTVar $ liftM protocols pi

-- Add protocols to a peer in the peerstore
addProtocols :: PeerId -> Peerstore -> [Protocol] -> STM ()
addProtocols pid ps protos = do
    mpi <- peerInfo pid ps
    case liftM protocols mpi of
        Just p -> writeTVar p (protos ++ p)

-- Make a new set of protocols to assign to the peerId: erase existing
setProtocols :: PeerId -> Peerstore -> [Protocol] -> STM ()
setProtocols pid ps protos = do
    mpi <- peerInfo pid ps
    case liftM protocols mpi of
        Just p -> writeTVar p protos

-- Determine from a set of protocols, which are supported by a given peer
supportsProtocols :: PeerId -> Peerstore -> [Protocol] -> STM [Protocol]
supportsProtocols pid ps protos = do
    supported <- getProtocols pid ps
    return $ protos `intersect` supported
