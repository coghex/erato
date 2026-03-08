{-# LANGUAGE Strict, UnicodeSyntax #-}

module Engine.Core.Queue
  ( Queue
  , newQueue
  , readQueue
  , writeQueue
  , tryReadQueue
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ

type Queue a = TQueue a

newQueue ∷ IO (Queue a)
newQueue = atomically TQ.newTQueue

readQueue ∷ Queue a → IO a
readQueue q = atomically (TQ.readTQueue q)

writeQueue ∷ Queue a → a → IO ()
writeQueue q a = atomically (TQ.writeTQueue q a)

tryReadQueue ∷ Queue a → IO (Maybe a)
tryReadQueue q = atomically (TQ.tryReadTQueue q)
