{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Adapter.HTTP.API.Types.Auth where


import Reexport 
import Domain.Auth
import Data.Aeson
import Adapter.HTTP.API.Types.AesonHelper



instance FromJSON Email where
  parseJSON = 
    withText "Email" $ withSmartConstructor mkEmail

instance FromJSON Password where
  parseJSON =
    withText "Password" $ withSmartConstructor mkPassword

$(fmap concat . sequence $ 
  [ deriveJSONRecord ''Auth
  , deriveToJSONUnwrap ''Email
  , deriveToJSONUnwrap ''Password
  , deriveJSONSumType ''RegistrationError
  , deriveJSONSumType ''EmailVerificationError
  , deriveJSONSumType ''LoginError
  ])
