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
    res      <- menuLivro livros
    salvarEmArquivoLivro "livros.txt" res

menuLivro :: [Livro] -> IO [Livro]
menuLivro livros = do
    -- menu de ações --
    putStrLn $ replicate 60 '\n' -- limpa tudo
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > Cadastrar livros"
    putStrLn "   2  > Cadastrar usuários"
    putStrLn "   3  > Empréstimo e devolução"
    putStrLn "   4  > Relatórios"
    putStrLn "   5  > Editar livro"
    putStrLn "   6  > Editar usuário"
    putStrLn "   7  > Salvar e Sair"
    putStrLn "- Digite o numero da ação: "
    input <- getLine
    
    case input of
        "1" -> do
            novosLivros <- adicionarLivroMenu livros
            menuLivro novosLivros

        "2" -> do
            menuLivro livros

        "3" -> do
            menuLivro livros

        "4" -> do
            menuLivro livros

        "5" -> do
            menuLivro livros

        "6" -> do
            menuLivro livros

        "7" -> return livros
        _ -> do
            putStrLn "Input inválido"
            menuLivro livros

adicionarLivroMenu :: [Livro] -> IO [Livro]
adicionarLivroMenu livros = do
    titulo <- inputString "Digite o título do livro: \n"
    autor  <- inputString "Digite o autor do livro: \n"
    ano    <- inputString "Digite o ano do livro: \n"
    cod    <- input "Digite o id do livro: \n" :: IO Int
    let status = Disponivel

    let novoLivro = Livro titulo autor ano cod status Nothing

    case adicionarlivro novoLivro livros of
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
    return livros

removerLivroMenu :: [Livro] -> IO [Livro]
removerLivroMenu livros = do
    return livros

adicionarUsuarioMenu :: [User] -> IO [User]
adicionarUsuarioMenu usuarios = do
    return usuarios

listarUsuariosMenu :: [User] -> IO [User]
listarUsuariosMenu usuarios = do
    return usuarios

removerUsuarioMenu :: [User] -> IO [User]
removerUsuarioMenu usuarios = do
    return usuarios

registrarEmprestimoMenu :: [Livro] -> IO [Livro]
registrarEmprestimoMenu livros = do
    return livros

registrarDevolucoesMenu :: [Livro] -> IO [Livro]
registrarDevolucoesMenu livros = do
    return livros

listaEsperaMenu :: [User] -> IO [User]
listaEsperaMenu usuarios = do
    return usuarios

exibirListaEsperaMenu :: [User] -> IO [User]
exibirListaEsperaMenu usuarios = do
    return usuarios

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





