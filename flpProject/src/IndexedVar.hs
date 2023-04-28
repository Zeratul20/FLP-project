module IndexedVar where

data IndexedVar = IndexedVar
  { ivName :: String
  , ivCount :: Int
  } deriving (Eq, Read, Show)

makeIndexedVar :: String -> IndexedVar
makeIndexedVar name = IndexedVar name 0

data Exp
  = X IndexedVar
  | Lam IndexedVar Exp
  | App Exp Exp
  deriving (Show)