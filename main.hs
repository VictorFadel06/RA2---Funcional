{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import DataTypes
import Logic
import Report
import System.IO
import System.Directory
import Control.Exception (catch, IOException)
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.Exception (evaluate)

inventoryFile :: FilePath
inventoryFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

main :: IO ()
main = do
    putStrLn "Iniciando sistema de inventário (RA2) - Haskell"
    setCurrentDirectoryIfNeeded
    inventario <- loadInventory
    putStrLn "Inventário carregado do arquivo:"
    printInventory inventario
    logs <- loadLogs
    loop inventario logs

setCurrentDirectoryIfNeeded :: IO ()
setCurrentDirectoryIfNeeded = return () -- noop para Online GDB / Repl.it

-- ============ Leitura do Inventário ============
loadInventory :: IO Inventario
loadInventory = catch readInv handler
  where
    readInv = do
      exists <- doesFileExist inventoryFile
      if not exists then return M.empty
      else do
        s <- readFile inventoryFile
        let parsed = reads s :: [(Inventario, String)]
        case parsed of
          [(inv, rest)] | all isSpace rest -> return inv
          _ -> case readMaybe s :: Maybe Inventario of
                 Just inv2 -> return inv2
                 Nothing -> do
                   putStrLn "Falha ao desserializar Inventario.dat — iniciando vazio"
                   return M.empty
    handler :: IOException -> IO Inventario
    handler _ = return M.empty

-- ============ Leitura dos Logs ============
loadLogs :: IO [LogEntry]
loadLogs = catch readLog handler
  where
    readLog = do
      exists <- doesFileExist logFile
      if not exists then return []
      else do
        s <- withFile logFile ReadMode $ \h -> do
          content <- hGetContents h
          evaluate (length content `seq` content)
        let ls = filter (not . all isSpace) (lines s)
        return $ mapMaybe readMaybe ls
    handler :: IOException -> IO [LogEntry]
    handler _ = return []

-- ============ Loop Principal ============
loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
    putStrLn ""
    putStrLn "Comandos: add | remove | update | list | report | help | exit"
    putStr ">>> "
    hFlush stdout
    cmd <- getLine
    now <- getCurrentTime
    case words cmd of
      ("add":_) -> do
        putStrLn "Formato: id nome quantidade categoria"
        putStr "id: "; hFlush stdout; i <- getLine
        putStr "nome: "; hFlush stdout; nm <- getLine
        putStr "quantidade: "; hFlush stdout; qS <- getLine
        putStr "categoria: "; hFlush stdout; cat <- getLine
        let qty = readSafeInt qS
        case qty of
          Nothing -> putStrLn "Quantidade inválida" >> loop inv logs
          Just q -> do
            let item = Item i nm q cat
            case addItem now item inv of
              Left err -> do
                let le = makeLog now (Add item) ("add " ++ i) (Falha err)
                appendLog le
                putStrLn $ "Erro: " ++ err
                loop inv (logs ++ [le])
              Right (inv', le) -> do
                safeWrite inventoryFile (show inv')
                appendLog le
                putStrLn "Item adicionado com sucesso."
                loop inv' (logs ++ [le])

      ("remove":rest) -> do
        (iid, qtyM) <- case rest of
          (x:y:_) -> return (x, readSafeInt y)
          (x:_) -> do putStr "quantidade: "; hFlush stdout; y <- getLine; return (x, readSafeInt y)
          _ -> do putStr "id: "; hFlush stdout; i <- getLine; putStr "quantidade: "; hFlush stdout; qS <- getLine; return (i, readSafeInt qS)
        case qtyM of
          Nothing -> putStrLn "Quantidade inválida" >> loop inv logs
          Just q -> do
            case removeItem now iid q inv of
              Left err -> do
                let le = makeLog now (Remove iid q) ("remove " ++ iid) (Falha err)
                appendLog le
                putStrLn $ "Erro: " ++ err
                loop inv (logs ++ [le])
              Right (inv', le) -> do
                safeWrite inventoryFile (show inv')
                appendLog le
                putStrLn "Remoção efetuada."
                loop inv' (logs ++ [le])

      ("update":rest) -> do
        (iid, qtyM) <- case rest of
          (x:y:_) -> return (x, readSafeInt y)
          (x:_) -> do putStr "nova quantidade: "; hFlush stdout; y <- getLine; return (x, readSafeInt y)
          _ -> do putStr "id: "; hFlush stdout; i <- getLine; putStr "nova quantidade: "; hFlush stdout; qS <- getLine; return (i, readSafeInt qS)
        case qtyM of
          Nothing -> putStrLn "Quantidade inválida" >> loop inv logs
          Just q -> do
            case updateQty now iid q inv of
              Left err -> do
                let le = makeLog now (Update iid q) ("update " ++ iid) (Falha err)
                appendLog le
                putStrLn $ "Erro: " ++ err
                loop inv (logs ++ [le])
              Right (inv', le) -> do
                safeWrite inventoryFile (show inv')
                appendLog le
                putStrLn "Atualização efetuada."
                loop inv' (logs ++ [le])

      ("list":_) -> do
        putStrLn "Inventário atual:"
        printInventory inv
        loop inv logs

      ("report":_) -> do
        putStrLn "Relatórios disponíveis: errors | historico <id> | maismovimentado"
        putStr "> "; hFlush stdout
        sub <- getLine
        case words sub of
          ("errors":_) -> do
            putStrLn "Logs de erro:"
            mapM_ print (logsDeErro logs)
            loop inv logs
          ("historico":iid:_) -> do
            putStrLn $ "Histórico do item " ++ iid ++ ":"
            mapM_ print (historicoPorItem iid logs)
            loop inv logs
          ("maismovimentado":_) -> do
            putStrLn "Item mais movimentado:"
            print (itemMaisMovimentado logs)
            loop inv logs
          _ -> putStrLn "Comando de report inválido." >> loop inv logs

      ("help":_) -> putStrLn helpText >> loop inv logs
      ("exit":_) -> putStrLn "Saindo..." >> return ()
      _ -> putStrLn "Comando não reconhecido." >> loop inv logs

-- ============ Utilitários ============

readSafeInt :: String -> Maybe Int
readSafeInt s = case reads s of
  [(n,"")] -> Just n
  _ -> Nothing

appendLog :: LogEntry -> IO ()
appendLog le = appendFile logFile (show le ++ "\n")

makeLog :: UTCTime -> AcaoLog -> String -> StatusLog -> LogEntry
makeLog ts a d s = LogEntry ts a d s

printInventory :: Inventario -> IO ()
printInventory inv = mapM_ print $ M.elems inv

safeWrite :: FilePath -> String -> IO ()
safeWrite path content = catch (writeFile path content) handler
  where handler :: IOException -> IO ()
        handler _ = putStrLn $ "Aviso: não foi possível gravar em " ++ path ++ " (arquivo em uso?)"

helpText :: String
helpText = unlines
  [ "add    - adiciona item (interativo)"
  , "remove - remove quantidade de item"
  , "update - altera quantidade"
  , "list   - lista inventário"
  , "report - gera relatórios a partir do log"
  , "help   - este texto"
  , "exit   - encerra"
  ]
