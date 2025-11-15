module Main where

import System.IO
import Control.Exception (catch, IOException)
import Data.Time (getCurrentTime, UTCTime)
import qualified Data.Map as Map




import LogicaPura
import Tipos
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)


import Data.Char (isDigit)
-----------------------------------------------------------
-- MAIN (único ponto de IO do programa)
-----------------------------------------------------------
-- Imprime um menu de ajuda com os comandos disponíveis
printMenu :: IO ()
printMenu = do
    putStrLn "Comandos disponíveis:"
    putStrLn "  add <id> <nome> <categoria> <quantidade>    -- Adiciona um item" 
    putStrLn "  remove <id>                                 -- Remove um item (pelo id)"
    putStrLn "  update <id> <nome> <categoria> <quantidade> -- Atualiza um item (pelo id)"
    putStrLn "  report                                      -- Gera relatórios do inventário e logs"
    putStrLn "  help | ?                                    -- Mostra este menu"
    putStrLn "  sair                                         -- Encerra o programa"

-- Gera uma String legível para o inventário
prettyInventario :: Inventario -> String
prettyInventario inv =
    let itensList = Map.elems (itens inv)
        format item = "- ID: " ++ itemID item ++ ", Nome: " ++ nome item ++ ", Cat: " ++ categoria item ++ ", Qtde: " ++ show (quantidade item)
    in if null itensList
       then "(Inventário vazio)"
       else unlines (map format itensList)

executarRelatorios :: IO ()
executarRelatorios = do
    putStrLn "\n=== RELATÓRIO DE LOGS ===\n"

    conteudo <- readFile "Auditoria.log"

    -- Aqui está a correção
    let logs = mapMaybe tryReadLog (lines conteudo)

    putStrLn "→ Logs de erro:"
    mapM_ print (logsDeErro logs)

    putStrLn "\n→ Item mais movimentado:"
    case itemMaisMovimentado logs of
        Nothing -> putStrLn "Nenhum item encontrado."
        Just (id, n) ->
            putStrLn ("Item " ++ id ++ " com " ++ show n ++ " movimentações.")

    putStrLn "\nDigite um ID para ver o histórico:"
    idBusca <- getLine

    putStrLn ("\n→ Histórico do item " ++ idBusca ++ ":")
    mapM_ print (historicoPorItem idBusca logs)

    putStrLn "\n=== FIM DO RELATÓRIO ===\n"

main :: IO ()
main = do
    -- Inicialização (IO puro)
    inventarioInicial <- catch
        (readFile "Inventario.dat" >>= (\txt -> return (read txt)))
        (\(_ :: IOException) -> do
            putStrLn "Inventário não encontrado. Criando novo."
            return (Inventario Map.empty))

    -- Loop principal como função local recursiva
    let loop inventario = do
            putStr "> "
            hFlush stdout
            entrada <- getLine

            case words entrada of
                ["sair"] -> putStrLn "Encerrando..."

                ("help":_) -> do
                    printMenu
                    loop inventario

                ("?":_) -> do
                    printMenu
                    loop inventario

                ("add":id:nome:cat:qtd:_) -> do
                    hora <- getCurrentTime
                    let item = Item id nome cat (read qtd)
                    case addItem hora item inventario of
                        Left erro -> do
                            putStrLn erro
                            appendFile "Auditoria.log" (erro ++ "\n")
                            loop inventario
                        Right (novoInv, logEntry) -> do
                            writeFile "Inventario.dat" (show (novoInv :: Inventario))
                            appendFile "Auditoria.log" (show logEntry ++ "\n")
                            putStrLn ("[" ++ show (acao logEntry) ++ "] " ++ detalhes logEntry)
                            putStrLn "Inventário atual:"
                            putStrLn (prettyInventario novoInv)
                            loop novoInv

                ("remove":id:qtd:_) -> do
                    hora <- getCurrentTime
                    let item = Item id "" "" (read qtd)
                    case removeItem hora item inventario of
                        Left erro -> do
                            putStrLn erro
                            appendFile "Auditoria.log" (erro ++ "\n")
                            loop inventario
                        Right (novoInv, logEntry) -> do
                            writeFile "Inventario.dat" (show novoInv)
                            appendFile "Auditoria.log" (show logEntry ++ "\n")
                            putStrLn ("[" ++ show (acao logEntry) ++ "] " ++ detalhes logEntry)
                            putStrLn "Inventário atual:"
                            putStrLn (prettyInventario novoInv)
                            loop novoInv

                ("update":id:nome:cat:qtd:_) -> do
                    hora <- getCurrentTime
                    let item = Item id nome cat (read qtd)
                    case updateItem hora item inventario of
                        Left erro -> do
                            putStrLn erro
                            appendFile "Auditoria.log" (erro ++ "\n")
                            loop inventario
                        Right (novoInv, logEntry) -> do
                            writeFile "Inventario.dat" (show novoInv)
                            appendFile "Auditoria.log" (show logEntry ++ "\n")
                            putStrLn ("[" ++ show (acao logEntry) ++ "] " ++ detalhes logEntry)
                            putStrLn "Inventário atual:"
                            putStrLn (prettyInventario novoInv)
                            loop novoInv
                            
                ("report":_) -> do
                    hora <- getCurrentTime
                    executarRelatorios
                    loop inventario

                _ -> do
                    appendFile "Auditoria.log"
                        ("Comando inválido: " ++ entrada ++ "\n")
                    loop inventario

    -- Mostra o menu de ajuda ao iniciar
    putStrLn "Bem-vindo ao gerenciador de inventário. Digite 'help' para ver os comandos." 
    printMenu
    -- Inicia o loop com o inventário carregado
    loop inventarioInicial
