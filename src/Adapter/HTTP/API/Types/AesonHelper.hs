module Adapter.HTTP.API.Types.AesonHelper where

import Reexport
import Data.Aeson.TH
import Data.Aeson.Types
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import Data.List (intercalate)
import ClassyPrelude (toLower)



withSmartConstructor :: (a -> Either [Text] b) -> a -> Parser b
withSmartConstructor constructor a =
  case constructor a of
    Left errs -> fail $ intercalate ". " . map T.unpack $ errs 
    Right val -> pure val


deriveJSONRecord :: Name -> Q [Dec]
deriveJSONRecord record =
  let lowerCaseFirst (y:ys) = toLower [y] <> ys
      lowerCaseFirst "" = ""
      
      structName = nameBase record
      opts = defaultOptions {
        fieldLabelModifier  = lowerCaseFirst . drop (length structName)
        }
  in deriveJSON opts record


deriveJSONSumType :: Name -> Q [Dec]
deriveJSONSumType record = 
  let structName = nameBase record
      opts = defaultOptions 
        { constructorTagModifier = drop (length structName)
        , tagSingleConstructors = True
        } 
  in deriveJSON opts record


deriveToJSONUnwrap :: Name -> Q [Dec]
deriveToJSONUnwrap = 
  let opts = defaultOptions {unwrapUnaryRecords = True}
  in deriveToJSON opts 