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
    putStrLn "======================================================================"
    putStrLn "                            Menu Principal                            "
    putStrLn "======================================================================"
    putStrLn "Opções:\n"
    putStrLn "  1 > Cadastrar livros"
    putStrLn "  2 > Cadastrar usuários"
    putStrLn "  3 > Empréstimo e devolução"
    putStrLn "  4 > Relatórios"
    putStrLn "  5 > Editar livro"
    putStrLn "  6 > Editar usuário"
    putStrLn "  0 > Salvar e Sair"
    input <- getLine
    case input of
        "1" -> do
            novosLivros <- adicionarLivroMenu livros
            menuPrincipal novosLivros usuarios registros
        "2" -> do
            novosUsuarios <- adicionarUsuarioMenu usuarios
            menuPrincipal livros novosUsuarios registros
        "3" -> do
            novosLivros <- menuLivro livros usuarios
            menuPrincipal novosLivros usuarios registros
        "4" -> do
            relatorio <- menuRelatorios livros registros
            menuPrincipal livros usuarios registros
        "5" -> do
            _ <- getLine
            menuPrincipal livros usuarios registros
        "6" -> do
            _ <- getLine
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
    putStrLn "   1  > Registrar empréstimo"
    putStrLn "   2  > Registrar devolução"
    putStrLn "   3  > Remover livro"
    putStrLn "   4  > Listar livros"
    putStrLn "   5  > Filtrar por disponibilidade"
    putStrLn "   6  > Mostrar lista de espera"
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

        "0" -> do
            return livros
              
        _ -> do
            putStrLn "Input inválido"
            menuLivro livros usuarios

menuUsuario :: [User] -> IO [User]
menuUsuario usuarios = do
    putStrLn $ replicate 60 '\n'
    putStrLn "   1  > Cadastrar usuarios"
    putStrLn "   2  > Listar usuários"
    putStrLn "   3  > Remover usuário"
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

        "0" -> return usuarios

        _ -> do
            putStrLn "input inválido"
            menuUsuario usuarios

menuRelatorios :: [Livro] -> [Registro] -> IO ()
menuRelatorios livros registros = do
    putStrLn "    1  > Listar empréstimos ativos"
    putStrLn "    2  > Histórico de empréstimos de um usuário"
    putStrLn "    3  > Livros com lista de espera"
    putStrLn "    0  > Voltar para o menu"
    input <- getLine
    case input of
        "1" -> do
            _ <- getLine
            menuRelatorios livros registros

        "2" -> do
            _ <- getLine
            menuRelatorios livros registros
            
        "0" -> return ()
        _ -> do
            putStrLn "input inválido"
            menuRelatorios livros registros
    
adicionarLivroMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
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

listarLivrosMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
listarLivrosMenu livros = do
    mapM_ (putStrLn . coutlivro) livros
    _ <- getLine
    return livros

removerLivroMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
removerLivroMenu livros = do
    cod <- input "Digite o id do livro a ser removido: " :: IO Int
    case removerLivro cod livros of
        Left msg -> do
            putStrLn msg
            return livros
        Right novosLivros -> do
            putStrLn "Livro removido com sucesso"
            _ <- getLine
            return novosLivros 

adicionarUsuarioMenu :: [User] -> IO [User] -- DOCUMENTADO --
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

listarUsuariosMenu :: [User] -> IO [User] -- DOCUMENTADO --
listarUsuariosMenu usuarios = do
    mapM_ (putStrLn . coutusuarios) usuarios
    _ <- getLine
    return usuarios

removerUsuarioMenu :: [User] -> IO [User] -- DOCUMENTADO --
removerUsuarioMenu usuarios = do
    mat <- input "Digite o número de matrícula do usuário: "
    case removerusuario mat usuarios of
        Left msg -> do
            putStrLn msg
            return usuarios
        Right novosUsuarios -> do
            putStrLn "Usuário removido com sucesso"
            _ <- getLine
            return novosUsuarios

registrarEmprestimoMenu :: [Livro] -> [User] -> IO [Livro] -- DOCUMENTADO --
registrarEmprestimoMenu livros usuarios = do
    idLivro <- input "Digite o id do livro: \n" :: IO Int
    matriculaUsuario <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let buscar = filter (\u -> matricula u == matriculaUsuario) usuarios
    case buscar of
        [] -> do
            putStrLn "Usuário não encontrado"
            _ <- getLine
            return livros
        (usuario:_) -> do
            resultado <- registraremprestimo idLivro usuario livros
            case resultado of
                Left erro -> do
                    putStrLn erro
                    _ <- getLine
                    return livros
                Right novosLivros -> do
                    putStrLn "Empréstimo concluido!"
                    _ <- getLine
                    return novosLivros



registrarDevolucoesMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
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

listarPorDisponibilidadeMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
listarPorDisponibilidadeMenu livros = do
    status <- input "Digite o status Disponivel | Indisponivel | Emprestado\n"
    let listaFiltrada = listarPorDisponibilidade status livros
    mapM_ (putStrLn.coutlivro) listaFiltrada
    _ <- getLine
    return livros

exibirListaEsperaMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
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



editarLivroMenu :: [Livro] -> IO [Livro]
editarLivroMenu books = do
    id <- input "Digite o id do livro a ser editado: \n" :: IO Int
    let bookLs = filter ((id==).cod) books
    if length bookLs /= 1 then do
        putStrLn "Erro, livro não encontrado"
        confirmation books
    else do
        let book = head bookLs
        putStrLn $ "Editando " ++ show book
        newTitle   <- inputString "Digite o novo título do livro: \n"
        newAuthor  <- inputString "Digite o novo autor do livro: \n"
        newYear    <- inputString "Digite o novo ano do livro: \n"
        let newBook  = book {titulo = newTitle, autor = newAuthor, ano = newYear}
        let newBooks = map (\l -> if cod l == id then newBook else l) books
        putStrLn "Livro editado com sucesso!"
        confirmation newBooks

editarUsuarioMenu :: [User] -> IO [User]
editarUsuarioMenu users = do
    id <- input "Digite o id do usuário a ser editado: \n" :: IO Int
    let userLs = filter ((id==).matricula) users
    if length userLs /= 1 then do
        putStrLn "Erro, usuário não encontrado"
        confirmation users
    else do
        let user = head userLs
        putStrLn $ "Editando " ++ show user
        newName  <- inputString "Digite o novo nome do usuário: \n"
        newEmail <- inputString "Digite o novo email do usuário: \n"
        let newUser  = user {nome = newName, email = newEmail}
        let newUsers = map (\u -> if matricula u == id then newUser else u) users
        putStrLn "Usuário editado com sucesso!"
        confirmation newUsers

-- Relatórios --

relatórioEmprestimosAtivosMenu :: [User] -> [Livro] -> [Registro] -> IO [Registro]
relatórioEmprestimosAtivosMenu us bs rs = do
    let regs = relatórioEmprestimosAtivos us bs rs
    putStrLn $ "Empréstimos ativos: " ++ show (length regs)
    mapM_ (putStrLn . coutregistros) regs
    confirmation rs



relatórioHistoricoMenu :: [User] -> [Registro] -> IO ()
relatórioHistoricoMenu users registers = do
    userId <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let userLs = filter ((userId==).matricula) users
    case userLs of
        [] -> do
            putStrLn "Id inválido!!"
            confirmation ()
        (_:_:_) -> do
            putStrLn "Multiplos Ids!!"
            confirmation ()
        [user] -> do
            let regs = relatórioHistorico user registers
            putStrLn $ "Histórico de " ++ show (nome user) ++ ":"
            putStrLn $ unlines $ map show regs
            confirmation ()