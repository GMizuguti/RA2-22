module Tipos where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
    
-- Tipos de dados

data Item = Item
    { itemID     :: String
    , nome       :: String
    , categoria  :: String
    , quantidade :: Int
    }deriving (Show, Read, Eq)

newtype Inventario = Inventario 
    { itens     :: Map.Map String Item }
    deriving (Show, Read, Eq)

data AcaoLog
    = Add
    | Remove
    | Update
    | QueryFail
    deriving (Show, Read, Eq)



data StatusLog 
    = Sucesso
    | Falha String
    deriving (Show, Read, Eq)



data LogEntry = LogEntry
    { timeStamp :: UTCTime
    , acao      :: AcaoLog
    , detalhes  :: String
    , status    :: StatusLog
    }
    deriving (Show, Read, Eq)