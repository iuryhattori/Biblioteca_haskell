module Persistencias(
    salvarEmArquivoLivro,
    carregarDeArquivoLivro,
    carregarDeArquivoUser,
    salvarEmArquivoUser
) where
import Tipos
import Funcoes
import System.IO
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Exception (evaluate)

stringParaLivro :: String -> Maybe Livro
stringParaLivro = readMaybe

stringParaUser :: String -> Maybe User
stringParaUser = readMaybe

carregarDeArquivoLivro :: FilePath -> IO [Livro]
carregarDeArquivoLivro ca = do
    co <- readFile ca
    let l = lines co
        livros = mapMaybe stringParaLivro l
    evaluate (length livros)
    return livros

salvarEmArquivoLivro :: FilePath -> [Livro] -> IO ()
salvarEmArquivoLivro ca co = do
    withFile ca WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle.show) co

carregarDeArquivoUser :: FilePath -> IO [User]
carregarDeArquivoUser ca = do
    co <- readFile ca
    let l = lines co
        usuarios = mapMaybe stringParaUser l
    evaluate (length usuarios)
    return usuarios

salvarEmArquivoUser :: FilePath -> [User] -> IO ()
salvarEmArquivoUser ca co = do
    withFile ca WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle.show) co

