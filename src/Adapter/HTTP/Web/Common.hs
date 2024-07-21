module Adapter.HTTP.Web.Common where


import Reexport
import Web.Scotty.Trans
import Domain.Auth
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Aeson as DF
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Adapter.HTTP.Common