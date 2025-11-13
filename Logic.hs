module Logic where

import qualified Data.Map as M
import DataTypes
import Data.Time.Clock

-- ResultadoOperacao
type ResultadoOperacao = (Inventario, LogEntry)

-- addItem: se id já existe -> falha; se quantidade negativa -> falha
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem ts it inv =
  if quantidade it < 0 then Left "Quantidade negativa não permitida"
  else if M.member (itemID it) inv then Left "Item com mesmo ID já existe"
  else
    let inv' = M.insert (itemID it) it inv
        le = LogEntry ts (Add it) ("add " ++ itemID it) Sucesso
    in Right (inv', le)

-- removeItem: se item não existe -> falha; se estoque insuficiente -> falha; se qty <=0 -> falha
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem ts iid qty inv
  | qty <= 0 = Left "Quantidade a remover deve ser positiva"
  | not (M.member iid inv) = Left "Item não encontrado"
  | otherwise =
      let it = inv M.! iid
      in if quantidade it < qty
           then Left "Estoque insuficiente"
           else
             let it' = it { quantidade = quantidade it - qty }
                 inv' = if quantidade it' <= 0 then M.delete iid inv else M.insert iid it' inv
                 le = LogEntry ts (Remove iid qty) ("remove " ++ iid) Sucesso
             in Right (inv', le)

-- updateQty: atualiza a quantidade absoluta para um valor novo (se negativo -> falha)
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty ts iid newQty inv
  | newQty < 0 = Left "Quantidade não pode ser negativa"
  | not (M.member iid inv) = Left "Item não encontrado"
  | otherwise =
      let it = inv M.! iid
          it' = it { quantidade = newQty }
          inv' = M.insert iid it' inv
          le = LogEntry ts (Update iid newQty) ("update " ++ iid) Sucesso
      in Right (inv', le)
