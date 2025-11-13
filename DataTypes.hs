module DataTypes where

import qualified Data.Map as M
import Data.Time.Clock

-- Item: itemID, nome, quantidade, categoria
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

type Inventario = M.Map String Item

data AcaoLog =
    Add Item
  | Remove String Int
  | Update String Int
  | Query String
  | Report
  deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Eq)
