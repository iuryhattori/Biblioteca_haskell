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
    let registros = [] -- você pode carregar de arquivo se quiser
    (livrosAtt, usuariosAtt, _) <- menuPrincipal livros usuarios registros
    salvarEmArquivoUser "usuarios.txt" usuariosAtt
    salvar_criarBiblioteca livrosAtt

menuPrincipal :: [Livro] -> [User] -> [Registro] -> IO ([Livro], [User], [Registro])
menuPrincipal livros usuarios registros = do
    putStrLn $ replicate 60 '\n'
    putStrLn "======================================================================"
    putStrLn "                            Menu Principal                            "
    putStrLn "======================================================================"
    putStrLn "Opções:\n"
    putStrLn "  1 > Cadastrar livros"
    putStrLn "  2 > Cadastrar usuários"
    putStrLn "  3 > Empréstimo e devolução"
    putStrLn "  4 > Relatórios"
    putStrLn "  0 > Salvar e Sair"
    putStr "- Escolha uma opção: "
    input <- getLine
    case input of
        "1" -> do
            putStrLn "Digite título, autor, ano e código do livro:"
            titulo <- getLine
            autor <- getLine
            ano <- readLn
            codigo <- readLn
            let novoLivro = Livro titulo autor ano codigo Disponivel Nothing []
            case adicionarlivro novoLivro livros of
                Left erro -> putStrLn erro >> menuPrincipal livros usuarios registros
                Right novaLista -> menuPrincipal novaLista usuarios registros

        "2" -> do
            putStrLn "Digite nome, matrícula e email do usuário:"
            nome <- getLine
            matricula <- readLn
            email <- getLine
            let novoUser = User nome matricula email
            case adicionarusuario novoUser usuarios of
                Left erro -> putStrLn erro >> menuPrincipal livros usuarios registros
                Right novaLista -> menuPrincipal livros novaLista registros

        "3" -> do
            novosLivros <- menuLivro livros usuarios registros
            menuPrincipal novosLivros usuarios registros

        "4" -> do
            menuRelatorios livros usuarios registros
            menuPrincipal livros usuarios registros

        "0" -> return (livros, usuarios, registros)

        _ -> do
            putStrLn "Opção inválida"
            menuPrincipal livros usuarios registros

menuLivro :: [Livro] -> [User] -> [Registro] -> IO [Livro]
menuLivro livros usuarios registros = do
    putStrLn $ replicate 60 '\n'
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > Registrar empréstimo"
    putStrLn "   2  > Registrar devolução"
    putStrLn "   3  > Remover livro"
    putStrLn "   4  > Filtrar por disponibilidade"
    putStrLn "   5  > Mostrar lista de espera"
    putStrLn "   0  > Voltar para o menu principal"
    putStr "- Digite o número da ação: "
    input <- getLine
    case input of
        "1" -> do
            putStrLn "ID do livro:"
            idlivro <- readLn
            putStrLn "Matrícula do usuário:"
            iduser <- readLn
            case find (\u -> matricula u == iduser) usuarios of
                Nothing -> putStrLn "Usuário não encontrado" >> menuLivro livros usuarios registros
                Just user -> do
                    resultado <- registraremprestimo idlivro user livros
                    case resultado of
                        Left erro -> putStrLn erro >> menuLivro livros usuarios registros
                        Right novaLista -> menuLivro novaLista usuarios registros

        "2" -> do
            putStrLn "ID do livro:"
            idlivro <- readLn
            putStrLn "ID do usuário:"
            iduser <- readLn
            case registrarDevolucao iduser idlivro registros of
                Left erro -> putStrLn erro >> return livros
                Right registrosAtualizados -> do
                    putStrLn "Devolução registrada."
                    menuLivro livros usuarios registrosAtualizados

        "3" -> do
            putStrLn "ID do livro a remover:"
            idlivro <- readLn
            case removerLivro idlivro livros registros of
                Left erro -> putStrLn erro >> menuLivro livros usuarios registros
                Right (livrosAtualizados, registrosAtualizados) -> menuLivro livrosAtualizados usuarios registrosAtualizados

        "4" -> do
            putStrLn "Deseja ver livros com qual status? (1: Disponível, 2: Emprestado, 3: Indisponível)"
            op <- getLine
            let status = case op of
                            "1" -> Disponivel
                            "2" -> Emprestado
                            "3" -> Indisponivel
                            _   -> Disponivel
            let filtrados = listarPorDisponibilidade status livros
            mapM_ (putStrLn . coutlivro) filtrados
            menuLivro livros usuarios registros

        "5" -> do
            putStrLn "Digite o código do livro:"
            codlivro <- readLn
            case find (\l -> cod l == codlivro) livros of
                Nothing -> putStrLn "Livro não encontrado" >> menuLivro livros usuarios registros
                Just livro -> putStrLn (exibirlistaespera livro) >> menuLivro livros usuarios registros

        "0" -> return livros

        _ -> do
            putStrLn "Input inválido"
            menuLivro livros usuarios registros

menuRelatorios :: [Livro] -> [User] -> [Registro] -> IO ()
menuRelatorios livros usuarios registros = do
    putStrLn "\n--- Menu de Relatórios ---"
    putStrLn "    1  > Listar empréstimos ativos"
    putStrLn "    2  > Histórico de empréstimos de um usuário"
    putStrLn "    3  > Livros com lista de espera"
    putStrLn "    0  > Voltar para o menu"
    input <- getLine
    case input of
        "1" -> do
            let rel = relatórioEmprestimosAtivos livros usuarios registros
            mapM_ (\(l, u) -> putStrLn $ coutlivro l ++ "\nEmprestado para: " ++ nome u) rel
            menuRelatorios livros usuarios registros

        "2" -> do
            putStrLn "Digite a matrícula do usuário:"
            mat <- readLn
            case find (\u -> matricula u == mat) usuarios of
                Nothing -> putStrLn "Usuário não encontrado" >> menuRelatorios livros usuarios registros
                Just u -> mapM_ print (relatórioHistorico u registros) >> menuRelatorios livros usuarios registros

        "3" -> do
            mapM_ (\l -> putStrLn ("Livro: " ++ titulo l ++ "\n" ++ exibirlistaespera l)) livros
            menuRelatorios livros usuarios registros

        "0" -> return ()

        _ -> putStrLn "input inválido" >> menuRelatorios livros usuarios registros
