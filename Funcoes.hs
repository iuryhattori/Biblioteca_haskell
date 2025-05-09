{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Funcoes (
    adicionarlivro,
    coutlivro,
    removerLivro,
    adicionarusuario,
    coutusuarios,
    removerusuario,
    listaespera,
    exibirlistaespera,
    registraremprestimo,
    registrarDevolucao,
    listarPorDisponibilidade,
    relatórioHistorico,
    relatórioEmprestimosAtivos,
    registrarDevolucoes
) where
import Tipos
import Data.Char

-- pega um livro e uma lista e contatena o livro com a lista
adicionarlivro:: Livro -> [Livro] -> Either String [Livro]
adicionarlivro liv x =  if elem liv x
                        then Left "Erro! Livro já registrado"
                        else Right (liv : x)
-- função auxiliar para imprimir um livro
coutlivro :: Livro -> String
coutlivro mostrarlivro =
        "Título: " ++ titulo mostrarlivro ++ "; " ++ 
        "Autor: " ++ autor mostrarlivro ++ "; " ++
        "Ano: " ++ show (ano mostrarlivro) ++ "; " ++
        "Código Único: " ++ show (cod mostrarlivro) ++ "; " ++
        "Status: " ++ show (status mostrarlivro) ++ "; " ++
        donoStr
    where
        donoStr = case dono mostrarlivro of
            Nothing  -> "Dono: Nenhum"
            Just usr -> "Dono: " ++ nome usr ++ " (" ++ show (matricula usr) ++ ")"
-- remove um livro com base num id de entrada
removerLivro :: Int -> [Livro] -> [Registro] -> Either String ([Livro], [Registro])
removerLivro id livros registros =
    if any (\t -> cod t == id) livros
        then let
                livrosAtualizados = filter (\p -> cod p /= id) livros
                registrosAtualizados = filter (\r -> livroId r /= id) registros
             in Right (livrosAtualizados, registrosAtualizados)
        else Left "Erro! Livro não registrado!"



-- recebe um usuário e uma lista de usuários e concatena o usuário com a lista
adicionarusuario :: User -> [User] -> Either String [User]
adicionarusuario us x = if elem us x
                        then Left "Erro! Usuário já cadastrado"
                        else Right (us : x)
-- função auxiliar para imprimir um usuário
coutusuarios :: User -> String
coutusuarios mostrarUser =
    "Nome: " ++ show (nome mostrarUser) ++ "; " ++ 
    "Matrícula: " ++ show ( matricula mostrarUser) ++ "; " ++
    "Email: " ++ show (email mostrarUser) 
-- remove um usuário com base num inteiro de entrada
removerusuario :: Int -> [User] -> [Livro] -> [Registro] -> Either String ([User], [Livro], [Registro])
removerusuario us usuarios livros registros = do
    let u = head $ filter ((us==).matricula) usuarios
    let id = matricula u
    if elem u usuarios then do
            let usuariosRestantes = filter ((us/=).matricula) usuarios
            let livrosAtualizados = map (removerDaFila u) livros
            let registrosAtualizados = filter ((/= id).usuarioId) registros
            Right (usuariosRestantes, livrosAtualizados, registrosAtualizados)
        else Left "Erro! Usuário não cadastrado"

removerDaFila :: User -> Livro -> Livro
removerDaFila us livro = livro { dono = sHead, fila = sTail, status = if null users then Disponivel else Emprestado }
        where
            users = case dono livro of 
                Just d  -> d:fila livro
                Nothing -> fila livro
            (sHead, sTail) = case filter (/=us) users of
                []     -> (Nothing, [])
                (x:xs) -> (Just x, xs)


registrarDevolucoes :: Int -> [Livro] -> [Registro] -> Either String ([Livro], [Registro])
registrarDevolucoes t livros registros =
    if t `elem` map cod livros
    then Right (map livroAtt livros, registrosAtualizados)
    else Left "Erro, livro não encontrado"
  where
    registrosAtualizados = filter ((/= t) . livroId) registros
    livroAtt livro
        | cod livro == t =
            case dono livro of
                Just u  -> removerDaFila u livro
                Nothing -> livro
        | otherwise = livro




registraremprestimo :: Int -> User -> [Livro] -> [Registro] -> IO (Either String ([Livro], [Registro]))
registraremprestimo id user livros registros =
    case break (\l -> cod l == id) livros of
        (_, []) -> return $ Left "Erro: livro não encontrado"
        (antes, livro:depois) ->
            case status livro of
                Disponivel -> do
                    let livroEmprestado = livro {status = Emprestado, dono = Just user}
                    return $ Right (antes ++ [livroEmprestado] ++ depois, registros ++ [Registro (matricula user) id Emprestado])
                Emprestado -> do
                    putStrLn "Livro indisponível, gostaria de entrar na lista de espera? (S/N)"
                    resposta <- getLine
                    if map toUpper resposta == "S" then do
                        if user `elem` fila livro || Just user == dono livro then
                            return $ Left "Você já está na fila deste livro!"
                        else do
                            let novaFila = fila livro ++ [user]
                            let livroAtualizado = livro {fila = novaFila}
                            return $ Right (antes ++ [livroAtualizado] ++ depois, registros)
                    else
                        return $ Left "Ok!"
                Indisponivel -> return $ Left "Livro está indisponível"


-- recebe um inteiro e uma lista de livros e modifica um livro com base nesse inteiro
registrarDevolucao :: Int -> Int -> [Registro] -> Either String [Registro]
registrarDevolucao iduser idlivro registros =
    if any (\r -> livroId r == idlivro && usuarioId r == iduser && stat r == Emprestado) registros
    then 
        let registrosAtualizados = map (\r -> if livroId r == idlivro && usuarioId r == iduser 
                                                then r  
                                                else r) registros
        in Right registrosAtualizados
    else Left "Erro! Emprestimo não encontrado"

listarEmprestimosAtivos :: [Registro] -> [Registro]
listarEmprestimosAtivos registros = filter (\r -> stat r == Emprestado) registros

-- lista os elementos por disponibilidade
listarPorDisponibilidade :: Status -> [Livro] -> [Livro]
listarPorDisponibilidade sta lista = filter (\t -> status t == sta) lista
-- adiciona um usuário na lista de espera
listaespera :: User -> Fila -> Either String Fila
listaespera user queue =    if elem user (usuarios queue)
                            then Left "Erro! Usuário já está na fila"
                            else Right queue { usuarios = user : usuarios queue}
-- imprime a lista de espera
exibirlistaespera :: Livro -> String
exibirlistaespera livro =
    unlines (map coutusuarios (fila livro)) ++ "\nTotal de usuários na fila: " ++ show (length(fila livro))



            -- Relatórios --
-- utilitarias
regToBook :: Livro -> Registro -> Bool
regToBook b r = cod b == livroId r

regToUser :: User -> Registro -> Bool
regToUser u r = matricula u == usuarioId r

regActive :: Registro -> Bool
regActive r = stat r == Emprestado






relatórioEmprestimosAtivos :: [Livro] -> [User] -> [Registro] -> [(Livro, User)]
relatórioEmprestimosAtivos bs us rs = [(b, u) | b <- bs , u <- us , r <- rs, 
              regActive r,
              regToBook b r,
              regToUser u r]


relatórioHistorico :: User -> [Registro] -> [Registro]
relatórioHistorico u rs = [r | r <- rs, usuarioId r == matricula u]
