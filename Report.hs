module Report where

import DataTypes
import qualified Data.Map as M
import Data.List
import Data.Ord

-- Retorna somente logs que sejam falhas
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isFail
  where isFail (LogEntry _ _ _ (Falha _)) = True
        isFail _ = False

-- Histórico por item: filtra log entries contendo o ID no acao ou detalhes
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid = filter matches
  where
    matches (LogEntry _ (Add it) _ _) = itemID it == iid
    matches (LogEntry _ (Remove idd _) _ _) = idd == iid
    matches (LogEntry _ (Update idd _) _ _) = idd == iid
    matches (LogEntry _ (Query idd) _ _) = idd == iid
    matches _ = False

-- Item mais movimentado: conta ocorrências por ID (Add/Remove/Update)
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let ids = concatMap extract logs
      counts = M.toList $ M.fromListWith (+) [(x,1) | x <- ids]
  in if null counts then Nothing else Just $ maximumBy (comparing snd) counts
  where
    extract (LogEntry _ (Add it) _ _) = [itemID it]
    extract (LogEntry _ (Remove idd _) _ _) = [idd]
    extract (LogEntry _ (Update idd _) _ _) = [idd]
    extract _ = []
