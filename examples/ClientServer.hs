{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Distributed.Closure
import Control.Distributed.Closure.TH
import Control.Monad (forever)
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Static
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.StaticPtr

-- | An instruction to the server.
data Instruction
  = CallStatic StaticKey Int
    -- ^ @CallStatic skFun arg@
    --
    -- Apply the function behind the 'StaticKey' @skFun@ to @arg@.
  | CallClosure (Closure (Int -> Int)) Int
    -- ^ @CallClosure cl arg@
    --
    -- Apply the closure @cl@ to @arg@.
  deriving Generic
instance Binary Instruction

-- | Handle an instruction by the client.
--
-- This is where we resolve a 'StaticKey'
-- by looking up the 'StaticPtr' and dereferencing it.
--
-- This is also where we resolve a 'Closure'.
handleInstruction :: Instruction -> IO (Maybe Int)
handleInstruction (CallStatic skey input) = do
  mbSPtr <- unsafeLookupStaticPtr skey
  return $ case mbSPtr of
    Nothing -> Nothing
    Just sptr ->
      let fun = deRefStaticPtr sptr in
      Just $ fun input
handleInstruction (CallClosure cl input) =
  let fun = unclosure cl in
  return $ Just $ fun input

-- | Channel to which a client will send its request.
type ServerChan = Chan (BSL.ByteString, ResponseChan)

-- | Channel to which the server will send its response.
type ResponseChan = Chan BSL.ByteString

-- | Execute an action with a concurrent server thread.
--
-- This mocks a network connection between a client and a server process.
-- For simplicity, the client and server run within the same process
-- and communicate through 'Chan's instead of sockets.
--
-- The server listens on a channel for requests.
-- The client sends requests on that channel together with a response channel.
-- The server handles the request and sends the result on the response channel.
withServer :: (ServerChan -> IO ()) -> IO ()
withServer action = do
  serverChan <- newChan
  let server = forever $ do
        (body, responseChan) <- readChan serverChan
        result <- case decodeOrFail body of
          Left _ -> return Nothing
          Right (_, _, instruction) -> handleInstruction instruction
        writeChan responseChan (encode result)
  withAsync server (\_ -> action serverChan)

-- | A global function that can be packed into a 'CallStatic' instruction.
double :: Int -> Int
double = (*2)

-- | A wrapper around 'Int' used to fulfill the 'Serializable' constraint,
-- so that it can be packed into a 'Closure'.
newtype SerializableInt = SI Int deriving (Generic, Typeable)
withStatic [d|
  instance Binary SerializableInt
  instance Typeable SerializableInt
  |]

-- | Demonstration of client server interactions.
main :: IO ()
main = withServer $ \serverChan -> do
  do
    clientChan <- newChan
    -- Obtain the 'StaticPtr' to the global function 'double'
    -- using the 'static' keyword, enabled by the 'StaticPointers' extension.
    -- Convert the 'StaticPtr' into a 'StaticKey' using 'staticKey',
    -- so that it can be sent across the wire.
    let fun = staticKey $ static double
        request = encode $ CallStatic fun 4
    writeChan serverChan (request, clientChan)
    result <- decode <$> readChan clientChan
    putStrLn $ "double 4 = " ++ show (result :: Maybe Int)
  do
    clientChan <- newChan
    -- Construct a 'Closure' that effectively captures a value
    -- and represents a partially applied function.
    -- The 'static' keyword is used to convert a lambda,
    -- that doesn't capture any free variables, into a 'StaticPtr'.
    -- Then we use 'staticMap' to partially apply the lambda within the closure.
    let three = SI 3
        c = static (\(SI a) b -> a + b)
          `staticMap` cpure closureDict three
        request = encode $ CallClosure c 4
    writeChan serverChan (request, clientChan)
    result <- decode <$> readChan clientChan
    putStrLn $ "3 + 4 = " ++ show (result :: Maybe Int)
