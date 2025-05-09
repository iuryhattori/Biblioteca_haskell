module Persistencias (
    salvarEmArquivo,
    carregarDeArquivo,
    safeSave
) where

import Tipos
import System.IO
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Exception (evaluate)
import System.Directory (doesFileExist)
import Data.Char (toUpper)

safeSave :: [a] -> (FilePath -> [a] -> IO ()) -> String -> IO ()
safeSave content func nome = do
    existe <- doesFileExist nome
    if existe then do
        putStrLn $ "Arquivo " ++ nome ++ " já existe, deseja sobrescrevê-lo? (S/N)"
        resposta <- getLine
        if map toUpper resposta == "S" then do
            func nome content
            putStr ""
        else
            putStr ""
    else do
        putStrLn $ "O arquivo " ++ nome ++ " não existe, deseja criar um novo? (S/N)"
        resposta <- getLine
        if map toUpper resposta == "S" then do
            func nome content
            putStr ""
        else
            putStr ""

salvarEmArquivo :: Show a => FilePath -> [a] -> IO ()
salvarEmArquivo ca list = do
    withFile ca WriteMode $ \h -> mapM_ (hPrint h) list


carregarDeArquivo :: Read a => FilePath -> IO [a]
carregarDeArquivo ca = do
    existe <- doesFileExist ca
    if not existe
        then return []
    else do
        co <- readFile ca
        let l = lines co
            list = mapMaybe readMaybe l
        evaluate (length list)
        return list