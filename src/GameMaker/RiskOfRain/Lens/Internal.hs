module GameMaker.RiskOfRain.Lens.Internal where

import           Data.Char
import           Data.Maybe
import           Data.List
import           Language.Haskell.TH
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude



makeFull :: String -> Name -> Q [Dec]
makeFull prefix name = makePartial prefix name $ const True



makePartial :: String -> Name -> (String -> Bool) -> Q [Dec]
makePartial prefix name include =
  let rules = camelCaseFields
                & lensField .~ partialNamer prefix include
  in makeLensesWith rules name



partialNamer :: String -> (String -> Bool) -> Name -> [Name] -> Name -> [DefName]
partialNamer prefix include _ _ field
  | include (ovr toLower base) =
      pure $ MethodName (mkName $ "Has" <> base) (mkName $ ovr toLower base)

  | otherwise = []
  where
    base = case stripPrefix prefix (nameBase field) of
             Nothing   -> error $ "Incorrect prefix (" <> prefix <> ") for field " <> nameBase field
             Just rest -> rest

    ovr f (a:as) = f a : as
    ovr _    []  =       []
