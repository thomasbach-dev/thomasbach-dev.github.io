module TextualRepresentationWithWitch.Example1 where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Gogol.FireStore as F

data Pet = Cat
         | Dog
         deriving (Eq, Show)

instance A.ToJSON Pet where
  toJSON Cat = A.String "cat"
  toJSON Dog = A.String "dog"

instance A.FromJSON Pet where
  parseJSON = A.withText "Pet" $
    \case
       "cat" -> pure Cat
       "dog" -> pure Dog
       s -> fail $ "Not a known pet: " <> show s

-- | Turn something into a field of a 'F.Document'.
class ToField a where
  toField :: a -> F.Value

instance ToField T.Text where
  toField v = F.newValue { F.stringValue = Just v }

instance ToField Pet where
  toField Cat = toField @T.Text "cat"
  toField Dog = toField @T.Text "dog"
