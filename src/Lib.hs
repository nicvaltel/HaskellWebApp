{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (runRoutine) where

import Reexport
import Domain.Auth
import qualified Adapter.InMemory.Auth as Mem
import Katip (KatipContextT)
import Logging (withKatip)


type MemState = TVar Mem.State

newtype App a = App { unApp :: ReaderT MemState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader MemState, MonadIO, MonadFail, KatipContext, Katip)


instance AuthRepo App where
  addAuth = App . Mem.addAuth
  setEmailAsVerified = App . Mem.setEmailAsVerified
  findUserByAuth = App . Mem.findUserByAuth
  findEmailFromUserId = App . Mem.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification email = App . Mem.notifyEmailVerification email

instance SessionRepo App where
  newSession = App . Mem.newSession
  findUserIdBySessionId = App . Mem.findUserIdBySessionId




runState :: LogEnv -> MemState -> App a -> IO a
runState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp
  
  
runRoutine :: IO ()
runRoutine = withKatip $ \le -> do 
  state <- newTVarIO Mem.initialState
  runState le state routine

routine :: App ()
routine = do
  let email = either undefined id $ mkEmail "salam@mail.md"
  let passw = either undefined id $ mkPassword "123456Hello"
  let auth = Auth email passw
  _ <- register auth
  Just vCode <- App $ Mem.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  liftIO $ print (session, uId, registeredEmail)
