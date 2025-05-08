module Persistencias (
    salvarEmArquivoLivro,  
    carregarDeArquivoLivro,
    carregarDeArquivoUser,
    salvarEmArquivoUser,
    salvar_criarBiblioteca
) where

import Tipos
import System.IO
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Exception (evaluate)
import System.Directory (doesFileExist)
import Data.Char (toUpper)

salvar_criarBiblioteca :: [Livro] -> IO ()
salvar_criarBiblioteca livros = do
    let nome = "biblioteca.txt"
    existe <- doesFileExist nome
    if existe then do
        putStrLn "Arquivo biblioteca.txt já existe, deseja sobrescrevê-lo? (S/N)"
        resposta <- getLine
        if map toUpper resposta == "S" then do
            salvarEmArquivoLivro nome livros
            putStrLn "Até a próxima"
        else
            putStrLn "Até a próxima"
    else do
        putStrLn "O arquivo biblioteca.txt não existe, deseja criar um novo? (S/N)"
        resposta <- getLine
        if map toUpper resposta == "S" then do
            salvarEmArquivoLivro nome livros
            putStrLn "Até a próxima"
        else
            putStrLn "Até a próxima"

stringParaLivro :: String -> Maybe Livro
stringParaLivro = readMaybe

stringParaUser :: String -> Maybe User
stringParaUser = readMaybe

salvarEmArquivoLivro :: FilePath -> [Livro] -> IO ()
salvarEmArquivoLivro ca livros = do
    withFile ca WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) livros

carregarDeArquivoLivro :: FilePath -> IO [Livro]
carregarDeArquivoLivro ca = do
    existe <- doesFileExist ca
    if not existe
        then return []
        else do
            co <- readFile ca
            let l = lines co
                livros = mapMaybe stringParaLivro l
            evaluate (length livros)
            return livros

carregarDeArquivoUser :: FilePath -> IO [User]
carregarDeArquivoUser ca = do
    co <- readFile ca
    let l = lines co
        usuarios = mapMaybe stringParaUser l
    evaluate (length usuarios)
    return usuarios

salvarEmArquivoUser :: FilePath -> [User] -> IO ()
salvarEmArquivoUser ca usuarios = do
    withFile ca WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) usuarios
