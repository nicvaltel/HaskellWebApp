{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (runRoutine) where

import Reexport
import Domain.Auth
import qualified Adapter.InMemory.Auth as Mem
import qualified Adapter.PostgreSQL.Auth as PG
import Katip (KatipContextT)
import Logging (withKatip)


type LibState = (PG.State, Mem.MemState)

newtype App a = App { unApp :: ReaderT LibState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader LibState, MonadIO, MonadFail, KatipContext, Katip)


instance AuthRepo App where
  addAuth = App . PG.addAuth
  setEmailAsVerified = App . PG.setEmailAsVerified
  findUserByAuth = App . PG.findUserByAuth
  findEmailFromUserId = App . PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification email = App . Mem.notifyEmailVerification email

instance SessionRepo App where
  newSession = App . Mem.newSession
  findUserIdBySessionId = App . Mem.findUserIdBySessionId




runState :: LogEnv -> LibState -> App a -> IO a
runState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp
  
  
runRoutine :: IO ()
runRoutine = withKatip $ \le -> do 
  memState <- newTVarIO Mem.initialState
  PG.withState pgCfg $ \pgState ->
    runState le (pgState, memState) routine
  where
    pgCfg = PG.Config
      { PG.configUrl = "postgresql://user:pass@localhost:6666/quorumdb"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.congigIdleConnTimeout = 10.0
      }

routine :: App ()
routine = do
  let email = either undefined id $ mkEmail "salam2@mail.md"
  let passw = either undefined id $ mkPassword "123456Hello"
  let auth = Auth email passw
  _ <- register auth
  Just vCode <- App $ Mem.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  liftIO $ print (session, uId, registeredEmail)
