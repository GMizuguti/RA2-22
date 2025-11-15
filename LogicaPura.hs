module LogicaPura where
import Tipos 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Char (isDigit)
import Data.List (maximumBy, isInfixOf)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Binary.Get (Decoder(Fail))

type ResultadoOperacao = (Inventario, LogEntry)

addItem     :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
removeItem  :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
updateItem  :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
preencher   :: String -> String

preencher idInicial
    | length idInicial >= 5 = idInicial
    | otherwise = replicate (5 - length idInicial) '0' ++ idInicial


addItem horaInsercao itemNovo inventario =
    let novoItem = 
            itemNovo { itemID = preencher (itemID itemNovo) }
    in
    case True of

    _   | length (itemID novoItem) > 5  ->
            Left (show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Id inserido maior que o permitido" ++ show novoItem
                    , status    = Falha "O id deve ter no máximo 5 caracteres."
                    }))


        | not (all isDigit (itemID novoItem)) ->
            Left (show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Id inserido continha caracteres invalidos " ++ show novoItem
                    , status    = Falha "O id deve conter apenas dígitos."
                    }))

        
        | quantidade novoItem < 0 ->
            Left ( show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Quantidade negativa inserida " ++ show novoItem
                    , status    = Falha "Quantidade do item não pode ser negativa."
                    }))


        | Map.member (itemID novoItem) (itens inventario) ->
            Left (show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Tentativa de adicionar item que já existe " ++ show novoItem
                    , status    = Falha ("Item com o ID " ++ itemID novoItem ++ " já existe.")
                    }))          


        | otherwise ->

            let mapaNovoInventario = Map.insert (itemID novoItem) novoItem (itens inventario)
                novoInventario = Inventario { itens = mapaNovoInventario }
                logEntradaSucesso = LogEntry
                    { timeStamp = horaInsercao
                    , acao = Add
                    , detalhes = "Adicionado item: " ++ show novoItem
                    , status = Sucesso
                    }
            in Right (novoInventario, logEntradaSucesso)



removeItem horaInsercao removeItem inventario =

    let itemRemover = 
            removeItem { itemID = preencher (itemID removeItem) }
    in
    case True of




    -- ID deve conter apenas dígitos
    _   | not (all isDigit (itemID itemRemover)) ->
            Left (show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Id requisitado continha caracteres invalidos " ++ show itemRemover
                    , status    = Falha "O id deve conter apenas dígitos."
                    }))

        -- Se o item NÃO existe no inventário
        | not (Map.member (itemID itemRemover) (itens inventario)) ->
            Left ( show (LogEntry
                    { timeStamp = horaInsercao
                    , acao      = QueryFail
                    , detalhes  = "Tentativa de remover item que não existe " ++ show itemRemover
                    , status    = Falha ("Item com o ID " ++ itemID itemRemover ++ " não existe.")
                    }))

        -- Caso exista, remove
        | otherwise ->
            case Map.lookup (itemID itemRemover) (itens inventario) of
                Just itemAtual ->
                    let qtdAtual   = quantidade itemAtual
                        qtdRemover = quantidade itemRemover
                    in
                    if qtdRemover <= 0 then
                        Left (show (LogEntry
                                { timeStamp = horaInsercao
                                , acao      = QueryFail
                                , detalhes  = "Tentativa de remover quantidade inválida para o item: " ++ show itemRemover
                                , status    = Falha "Quantidade a remover deve ser maior que zero."
                                }))

                    else if qtdRemover > qtdAtual then
                        Left ( show (LogEntry
                                { timeStamp = horaInsercao
                                , acao      = QueryFail
                                , detalhes  = "Tentativa de remover mais unidades do que o disponível para o item: " ++ show itemRemover
                                , status    = Falha ("Quantidade a remover (" ++ show qtdRemover ++ ") maior que a disponível (" ++ show qtdAtual ++ ").")
                                }))

                    else
                        let itemAtualizado =
                                itemAtual { quantidade = qtdAtual - qtdRemover }

                            mapaNovoInventario =
                                Map.insert (itemID itemRemover)
                                        itemAtualizado
                                        (itens inventario)

                            novoInventario = Inventario { itens = mapaNovoInventario }

                            logEntradaSucesso = LogEntry
                                { timeStamp = horaInsercao
                                , acao      = Remove
                                , detalhes  = "Removidas "
                                            ++ show qtdRemover
                                            ++ " unidades do item: "
                                            ++ itemID itemRemover
                                , status    = Sucesso
                                }
                        in Right (novoInventario, logEntradaSucesso)



updateItem horaInsercao itemNovo inventario =
    let novoItem = 
            itemNovo { itemID = preencher (itemID itemNovo) }
    in
    case True of

      
        -- ID não pode ser maior que 5 caracteres
    _   | length (itemID novoItem) > 5  ->
            Left ("\nFALHA NA OPERACAO ATUALIZAR\n" ++ 
                "O id deve ter no máximo 5 caracteres.\n" ++ 
                "Tentativa: de entrada" ++ show novoItem)
    
        -- ID deve conter apenas dígitos
        | not (all isDigit (itemID novoItem)) ->
            Left ("\nFALHA NA OPERACAO ATUALIZAR\n"
                ++ "O ID deve conter apenas dígitos.\n"
                ++ "Tentativa de atualização: " ++ show novoItem)
        
        -- o item deve existir para ser atualizado
        | not (Map.member (itemID novoItem) (itens inventario)) ->
            Left ("\nFALHA NA OPERACAO ATUALIZAR \n" ++
                "Item com o ID " ++ itemID novoItem ++ " não existe.\n" ++
                "Tentativa de atualização: " ++ show novoItem)

        -- Quantidade não pode ser negativa
        | quantidade novoItem < 0 ->
            Left ("\nFALHA NA OPERACAO ATUALIZAR \n" ++
                "Quantidade do item não pode ser negativa.\n" ++
                "Tentativa de atualização: " ++ show novoItem)


        -- nome, categoria e ID não podem ser vazios
        | null (nome novoItem)      -> Left "Nome do item não pode ser vazio."
        | null (categoria novoItem) -> Left "Categoria do item não pode ser vazia."
        | null (itemID novoItem)    -> Left "O id do item não pode ser vazio."

        -- Caso esteja tudo OK, atualizar o item
        | otherwise ->
            let mapaNovoInventario = Map.insert (itemID novoItem) novoItem (itens inventario) 

                novoInventario = Inventario { itens = mapaNovoInventario }

                logEntradaSucesso = LogEntry
                    { timeStamp = horaInsercao
                    , acao      = Update
                    , detalhes  = "Item atualizado: " ++ show novoItem
                    , status    = Sucesso
                    }
            in Right (novoInventario, logEntradaSucesso)




historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemID =
    filter (\log -> itemID `isInfixOf` detalhes log)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro =
    filter (\log -> case status log of
                       Falha _ -> True
                       _       -> False)




tryReadLog :: String -> Maybe LogEntry
tryReadLog linha = readMaybe linha

extrairIDs :: LogEntry -> [String]
extrairIDs log =
    filter (all isDigit) (words (detalhes log))

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
    let ids = concatMap extrairIDs logs
        freqMap = Map.fromListWith (+) [(i, 1) | i <- ids]
    in if Map.null freqMap
       then Nothing
       else Just (maximumBy (comparing snd) (Map.toList freqMap))