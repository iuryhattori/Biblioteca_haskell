module Main where

import System.IO
import Data.List

import Tipos
import Funcoes
import Persistencias


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    livros   <- carregarDeArquivoLivro "livros.txt"
    usuarios <- carregarDeArquivoUser "usuarios.txt"
    (livrosAtt, usuariosAtt) <- menuPrincipal livros usuarios
    salvarEmArquivoUser "usuarios.txt" usuariosAtt
    salvarEmArquivoLivro "livros.txt" livrosAtt

menuPrincipal :: [Livro] -> [User] -> IO ([Livro],[User])
menuPrincipal livros usuarios = do
    putStrLn $ replicate 60 '\n'
    putStrLn "   1 > Menu de livros"
    putStrLn "   2 > Menu de usuários"
    putStrLn "   3 > Salvar e sair"
    input <- getLine
    case input of
        "1" -> do
            novosLivros <- menuLivro livros usuarios
            menuPrincipal novosLivros usuarios
        "2" -> do
            novosUsuarios <- menuUsuario usuarios
            menuPrincipal livros novosUsuarios
        "3" -> return (livros, usuarios)
        _ -> do
            putStrLn "Opção inválida"
            menuPrincipal livros usuarios


menuLivro :: [Livro] -> [User] -> IO [Livro]
menuLivro livros usuarios = do
    -- menu de ações --
    putStrLn $ replicate 60 '\n' -- limpa tudo
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > Cadastrar livros"
    putStrLn "   2  > Cadastrar Empréstimo"
    putStrLn "   3  > Relatórios"
    putStrLn "   4  > Remover livro"
    putStrLn "   5  > Listar livros"
    putStrLn "   6  > Filtrar por disponibilidade"
    putStrLn "   7  > Mostrar lista de espera"
    putStrLn "   14 > Voltar para o menu principal"
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
            menuLivro livros usuarios

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
              
        "14" -> return livros

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
    putStrLn "   14 > Voltar para o menu"
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

        "14" -> return usuarios

        _ -> do
            putStrLn "input inválido"
            menuUsuario usuarios


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
    case livroParaRemover of
        [] -> do
            putStrLn "Erro, livro não encontrado"
            _ <- getLine
            return livros
        (liv:_) -> do
            case removerLivro liv livros of
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
        (usu:_) -> do
            case removerusuario usu usuarios of
                Left erro -> do
                    putStrLn erro
                    return usuarios
                Right novosUsuarios -> do
                    putStrLn "Usuário removido!"
                    _ <- getLine
                    return novosUsuarios 

registrarEmprestimoMenu :: [Livro] -> [User] -> IO [Livro]
registrarEmprestimoMenu livros usuarios = do
    tituloLivro <- inputString "Digite o título do livro: \n"
    matriculaUsuario <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let buscar = filter (\u -> matricula u == matriculaUsuario) usuarios
    case buscar of
        [] -> do
            putStrLn "Usuário não encontrado"
            _ <- getLine
            return livros
        (usuario:_) -> do
            resultado <- registraremprestimo tituloLivro usuario livros
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
    tituloLivro <- inputString "Digite o título do livro: \n"
    case registrardevolucoes tituloLivro livros of
        Left erro -> do
            putStrLn erro
            _ <- getLine
            return livros
        Right novosLivros -> do
            putStrLn "Evolução concluida!"
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
    putStrLn "Digite o título do livro para exibir a lista de espera: "
    tituloLivro <- inputString ""
    let filtraLivro = filter (\livro -> titulo livro == tituloLivro) livros
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


 



