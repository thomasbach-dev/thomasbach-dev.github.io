module TextualRepresentationWithWitch.Example2 where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Gogol.FireStore as F
import qualified Witch

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)

data Pet = Cat
         | Dog
         deriving (Eq, Show)

instance Witch.From Pet T.Text where
  from Cat = "cat"
  from Dog = "dog"

instance Witch.TryFrom T.Text Pet where
  tryFrom = Witch.maybeTryFrom convert
    where
     convert "cat" = pure Cat
     convert "dog" = pure Dog
     convert _ = Nothing

instance A.ToJSON Pet where
  toJSON = A.String . Witch.from

instance A.FromJSON Pet where
  parseJSON = A.withText "Pet" failFrom

instance Witch.From T.Text F.Value where
  from v = F.newValue { F.stringValue = Just v }

failFrom :: forall tgt src m. (Typeable tgt, MonadFail m, Witch.TryFrom src tgt)
         => src -> m tgt
failFrom source = case Witch.tryFrom source of
                    Right target -> pure target
                    Left _       -> fail $ "Not a valid " <> show (typeRep (Proxy @tgt))
