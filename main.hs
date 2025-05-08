module Main where

import System.IO
import Data.List

import Tipos
import Funcoes
import Persistencias


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    livros   <- carregarDeArquivoLivro "biblioteca.txt"
    usuarios <- carregarDeArquivoUser "usuarios.txt"
    let registros = []
    (livrosAtt, usuariosAtt, _) <- menuPrincipal livros usuarios registros
    salvarEmArquivoUser "usuarios.txt" usuariosAtt

menuPrincipal :: [Livro] -> [User] -> [Registro] -> IO ([Livro], [User], [Registro])
menuPrincipal livros usuarios registros = do
    putStrLn $ replicate 60 '\n'
    putStrLn "   1  > Menu de livros"
    putStrLn "   2  > Menu de usuários"
    putStrLn "   3  > Relatórios"
    putStrLn "   0  > Salvar e sair"
    input <- getLine
    case input of
        "1" -> do
            novosLivros <- menuLivro livros usuarios
            menuPrincipal novosLivros usuarios registros
        "2" -> do
            novosUsuarios <- menuUsuario usuarios
            menuPrincipal livros novosUsuarios registros
        "3" -> do
            menuRelatorios usuarios livros registros
            menuPrincipal livros usuarios registros
        "0" -> do
            salvar_criarBiblioteca livros
            return (livros, usuarios, registros)
        _ -> do
            putStrLn "Opção inválida"
            menuPrincipal livros usuarios registros


menuLivro :: [Livro] -> [User] -> IO [Livro]
menuLivro livros usuarios = do
    -- menu de ações --
    putStrLn $ replicate 60 '\n' -- limpa tudo
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > Cadastrar livros"
    putStrLn "   2  > Registrar empréstimo"
    putStrLn "   3  > Registrar devolução"
    putStrLn "   4  > Remover livro"
    putStrLn "   5  > Listar livros"
    putStrLn "   6  > Filtrar por disponibilidade"
    putStrLn "   7  > Mostrar lista de espera"
    putStrLn "   0  > Voltar para o menu principal"
    putStrLn "- Digite o numero da ação: "
    input <- getLine

    case input of
        "1" -> do
            novosLivros <- adicionarLivroMenu livros
            menuLivro novosLivros usuarios

        "2" -> do
            novosLivros <- registrarEmprestimoMenu livros usuarios
            menuLivro novosLivros usuarios

        "3" -> do
            novosLivros <- registrarDevolucoesMenu livros
            menuLivro novosLivros usuarios

        "4" -> do
            novosLivros <- removerLivroMenu livros
            menuLivro novosLivros usuarios

        "5" -> do
            listaLivros <- listarLivrosMenu livros
            menuLivro listaLivros usuarios

        "6" -> do
            listaFiltrada <- listarPorDisponibilidadeMenu livros
            menuLivro livros usuarios

        "7" -> exibirListaEsperaMenu livros

        "0" -> return livros

        _ -> do
            putStrLn "Input inválido"
            menuLivro livros usuarios

menuUsuario :: [User] -> IO [User]
menuUsuario usuarios = do
    putStrLn $ replicate 60 '\n'
    putStrLn "   1  > Cadastrar usuarios"
    putStrLn "   2  > Listar usuários"
    putStrLn "   3  > Remover usuário"
    putStrLn "   4  > Adicionar à lista de espera"
    putStrLn "   0  > Voltar para o menu"
    putStrLn "- Digite o numero da ação: "
    input <- getLine

    case input of
        "1" -> do
            novosUsuarios <- adicionarUsuarioMenu usuarios
            menuUsuario novosUsuarios

        "2" -> do
            listaUsuarios <- listarUsuariosMenu usuarios
            menuUsuario listaUsuarios

        "3" -> do
            novosUsuarios <- removerUsuarioMenu usuarios
            menuUsuario novosUsuarios
        -- TODO: implementar
        -- "4" -> do
        --     novosUsuarios <- adicionarUsuarioAEsperaMenu usuarios
        --     menuUsuario novosUsuarios

        "0" -> return usuarios

        _ -> do
            putStrLn "input inválido"
            _ <- getChar
            menuUsuario usuarios

menuRelatorios :: [User] -> [Livro] -> [Registro] -> IO ()
menuRelatorios usuarios livros registros = do
    putStrLn "    1  > Listar empréstimos ativos"
    putStrLn "    2  > Histórico de empréstimos de um usuário"
    putStrLn "    3  > Livros com lista de espera"
    putStrLn "    0  > Voltar para o menu"
    input <- getLine
    case input of
        "1" -> do
            menuRelatorios usuarios livros registros

        "2" -> do
            relatórioHistoricoMenu usuarios registros

        "0" -> return ()
        _ -> do
            putStrLn "input inválido"
            menuRelatorios usuarios livros registros

adicionarLivroMenu :: [Livro] -> IO [Livro]
adicionarLivroMenu livros = do
    titulo <- inputString "Digite o título do livro: \n"
    autor  <- inputString "Digite o autor do livro: \n"
    ano    <- inputString "Digite o ano do livro: \n"
    cod    <- input "Digite o id do livro: \n" :: IO Int
    let status = Disponivel

    let novo = Livro titulo autor ano cod status Nothing []

    case adicionarlivro novo livros of
        Left erro -> do
            putStrLn erro
            return livros
        Right novosLivros -> do
            putStrLn "Livro adicionado com sucesso!"
            _ <- getLine
            return novosLivros

listarLivrosMenu :: [Livro] -> IO [Livro]
listarLivrosMenu livros = do
    mapM_ (putStrLn . coutlivro) livros
    _ <- getLine
    return livros

removerLivroMenu :: [Livro] -> IO [Livro]
removerLivroMenu livros = do
    codLivro <- input "Digite o id do livro a ser removido: " :: IO Int
    let livroParaRemover = filter (\l -> cod l == codLivro) livros
    if length livroParaRemover /= 1 then do
            putStrLn "Erro, livro não encontrado"
            _ <- getLine
            return livros
    else do
        case removerLivro codLivro livros of
            Left erro -> do
                putStrLn erro
                return livros
            Right novosLivros -> do
                putStrLn "Livro removido!"
                _ <- getLine
                return novosLivros 

adicionarUsuarioMenu :: [User] -> IO [User]
adicionarUsuarioMenu usuarios = do
    nome       <- inputString "Digite o nome do usuario: \n"
    matricula  <- input "Digite o número de matricula do usuário: \n" :: IO Int
    email      <- inputString "Digite o email do usuário: \n"

    let novo = User nome matricula email

    case adicionarusuario novo usuarios of
        Left erro -> do
            putStrLn erro
            return usuarios
        Right novosUsuarios -> do
            putStrLn "Usuario cadastrado com sucesso!"
            _ <- getLine
            return novosUsuarios

listarUsuariosMenu :: [User] -> IO [User]
listarUsuariosMenu usuarios = do
    mapM_ (putStrLn . coutusuarios) usuarios
    _ <- getLine
    return usuarios

removerUsuarioMenu :: [User] -> IO [User]
removerUsuarioMenu usuarios = do
    idUsuario <- input "Digite o id do usuário a ser removido: " :: IO Int
    let usuarioParaRemover = filter (\l -> matricula l == idUsuario) usuarios
    case usuarioParaRemover of
        [] -> do
            putStrLn "Erro, usuário não encontrado"
            _ <- getLine
            return usuarios
        (_:_:_) -> do
            putStrLn "Multiplos usuarios!!"
            _ <- getLine
            return usuarios
        [usu] -> do
            case removerusuario idUsuario usuarios of
                Left erro -> do
                    putStrLn erro
                    return usuarios
                Right novosUsuarios -> do
                    putStrLn "Usuário removido!"
                    _ <- getLine
                    return novosUsuarios 

registrarEmprestimoMenu :: [Livro] -> [User] -> IO [Livro]
registrarEmprestimoMenu livros usuarios = do
    livroId <- input "Digite o id do livro: \n" :: IO Int
    usuarioId <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let usuarioLs = filter ((usuarioId==).matricula) usuarios
    let livroLs   = filter ((livroId==).cod)         livros
    if length usuarioLs /= 1 || length livroLs /= 1 then do
        putStrLn "Usuário não encontrado" -- tratar se achar mais de 1?
        _ <- getLine
        return livros
    else do
        resultado <- registraremprestimo livroId (head usuarioLs) livros
        case resultado of
            Left erro -> do
                putStrLn erro
                _ <- getLine
                return livros
            Right novosLivros -> do
                putStrLn "Empréstimo concluido!"
                _ <- getLine
                return novosLivros



registrarDevolucoesMenu :: [Livro] -> IO [Livro]
registrarDevolucoesMenu livros = do
    id <- input "Digite o código do livro: \n"
    case registrardevolucoes id livros of
        Left erro -> do
            putStrLn erro
            _ <- getLine
            return livros
        Right novosLivros -> do
            putStrLn "Devolução concluida!"
            _ <- getLine
            return novosLivros

listarPorDisponibilidadeMenu :: [Livro] -> IO [Livro]
listarPorDisponibilidadeMenu livros = do
    status <- input "Digite o status Disponivel | Indisponivel | Emprestado\n"
    let listaFiltrada = listarPorDisponibilidade status livros
    mapM_ (putStrLn.coutlivro) listaFiltrada
    _ <- getLine
    return livros

exibirListaEsperaMenu :: [Livro] -> IO [Livro]
exibirListaEsperaMenu livros = do
    putStrLn "Digite o código do livro para exibir a lista de espera: "
    id <- input ""
    let filtraLivro = filter (\livro -> cod livro == id) livros
    case filtraLivro of
        [] -> do
            putStrLn "Livro não encontrado"
            _ <- getLine
            return livros
        (livro:_) -> do
            putStrLn (exibirlistaespera livro)
            _ <- getLine
            return livros

inputString :: String -> IO String
inputString text = do
    putStr text
    line <- getLine
    return line

input :: Read a => String -> IO a
input text = do
    putStr text
    line <- getLine
    case reads line of
        [(x, "")] -> return x
        _      -> do
            putStr "Comando inválido!\n"
            input text


-- Relatórios --

relatórioHistoricoMenu :: [User] -> [Registro] -> IO ()
relatórioHistoricoMenu users registers = do
    userId <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let userLs = filter ((userId==).matricula) users
    case userLs of
        [] -> do
            putStrLn "Id inválido!!"
            _ <- getChar
            return ()
        (_:_:_) -> do
            putStrLn "Multiplos Ids!!"
            _ <- getChar
            return ()
        [user] -> do
            let regs = relatórioHistorico user registers
            putStrLn $ "Histórico de " ++ show (nome user) ++ ":"
            putStrLn $ unlines $ map show regs
            _ <- getChar
            return ()