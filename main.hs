module Main where

import System.IO
import Data.List

import Tipos
import Funcoes
import Persistencias

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    livros    <- carregarDeArquivo "biblioteca.txt" :: IO [Livro]
    usuarios  <- carregarDeArquivo "usuarios.txt"   :: IO [User]
    registros <- carregarDeArquivo "registros.txt"  :: IO [Registro]
    (livrosAtt, usuariosAtt, registrosAtt) <- menuPrincipal livros usuarios registros
    safeSave livrosAtt salvarEmArquivo "biblioteca.txt"
    safeSave usuariosAtt salvarEmArquivo "usuarios.txt"
    safeSave registrosAtt salvarEmArquivo "registros.txt"
    putStrLn "Até a próxima!"

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
    putStrLn "  7 > Salvar e Sair"
    putStr "- Escolha uma opção: "
    input <- getLine
    case input of
        "1" -> do
            novosLivros <- adicionarLivroMenu livros
            menuPrincipal novosLivros usuarios registros

        "2" -> do
            novosUsuarios <- adicionarUsuarioMenu usuarios
            menuPrincipal livros novosUsuarios registros

        "3" -> do
            (novosLivros, novosRegistros) <- menuLivro livros usuarios registros
            menuPrincipal novosLivros usuarios novosRegistros

        "4" -> do
            menuRelatorios livros usuarios registros
            menuPrincipal livros usuarios registros
        "5" -> do
            novosLivros <- editarLivroMenu livros
            menuPrincipal novosLivros usuarios registros
        "6" -> do
            (novosUsuarios, novosLivros) <- editarUsuarioMenu usuarios livros
            menuPrincipal novosLivros novosUsuarios registros
        "7" -> return (livros, usuarios, registros)

        _ -> do
            putStrLn "Opção inválida"
            menuPrincipal livros usuarios registros

menuLivro :: [Livro] -> [User] -> [Registro] -> IO ([Livro], [Registro])
menuLivro livros usuarios registros = do
    putStrLn $ replicate 60 '\n'
    putStrLn "- O que gostaria de realizar?:"
    putStrLn "   1  > Registrar empréstimo"
    putStrLn "   2  > Registrar devolução"
    putStrLn "   3  > Remover livro"
    putStrLn "   4  > Filtrar por disponibilidade"
    putStrLn "   5  > Mostrar lista de espera"
    putStrLn "   6  > Voltar para o menu principal"
    putStr "- Digite o número da ação: "
    option <- getLine
    case option of
        "1" -> do
            (novosLivros, novosRegistros) <- registrarEmprestimoMenu livros usuarios registros
            menuLivro novosLivros usuarios novosRegistros

        "2" -> do
            (novosLivros, novosRegistros) <- registrarDevolucoesMenu livros registros
            menuLivro novosLivros usuarios novosRegistros

        "3" -> do
            putStrLn "ID do livro a remover:"
            idlivro <- readLn
            case removerLivro idlivro livros registros of
                Left erro -> do
                    putStrLn erro
                    _ <- getChar
                    menuLivro livros usuarios registros
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
            _ <- getChar
            menuLivro livros usuarios registros

        "5" -> do
            codlivro <- input "Digite o código do livro:" :: IO Int
            case find (\l -> cod l == codlivro) livros of
                Nothing -> do
                    putStrLn "Livro não encontrado"
                    _ <- getChar
                    menuLivro livros usuarios registros
                Just livro -> do
                    putStrLn (exibirlistaespera livro)
                    _ <- getChar
                    menuLivro livros usuarios registros
        "6" -> return (livros, registros)

        _ -> do
            putStrLn "Input inválido"
            _ <- getChar
            menuLivro livros usuarios registros

menuRelatorios :: [Livro] -> [User] -> [Registro] -> IO ()
menuRelatorios livros usuarios registros = do
    putStrLn $ replicate 60 '\n'
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
            _ <- getChar
            menuRelatorios livros usuarios registros

        "2" -> do
            relatórioHistoricoMenu usuarios registros livros
            _ <- getChar
            menuRelatorios livros usuarios registros

        "3" -> do
            mapM_ (\l -> putStrLn ("Livro: " ++ titulo l ++ "\n" ++ exibirlistaespera l)) livros
            menuRelatorios livros usuarios registros

        "0" -> return ()

        _ -> putStrLn "input inválido" >> menuRelatorios livros usuarios registros
adicionarLivroMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
adicionarLivroMenu livros = do
    titulo <- inputString "Digite o título do livro: \n"
    autor  <- inputString "Digite o autor do livro: \n"
    ano    <- input "Digite o ano do livro: \n" :: IO Int
    cod    <- input "Digite o id do livro: \n" :: IO Int
    let status = Disponivel

    let novo = Livro titulo autor ano cod status Nothing []

    case adicionarlivro novo livros of
        Left erro -> do
            putStrLn erro
            confirmation livros
        Right novosLivros -> do
            putStrLn "Livro adicionado com sucesso!"
            confirmation novosLivros

listarLivrosMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
listarLivrosMenu livros = do
    mapM_ (putStrLn . coutlivro) livros
    confirmation livros

removerLivroMenu :: [Livro] -> [Registro] -> IO ([Livro], [Registro]) -- DOCUMENTADO --
removerLivroMenu livros registros = do
    codLivro <- input "Digite o id do livro a ser removido: " :: IO Int
    let livroParaRemover = filter (\l -> cod l == codLivro) livros
    if length livroParaRemover /= 1 then do
            putStrLn "Erro, livro não encontrado"
            confirmation (livros, registros)
    else do
        case removerLivro codLivro livros registros of
            Left erro -> do
                putStrLn erro
                confirmation (livros, registros)
            Right (novosLivros, novosRegistros) -> do
                putStrLn "Livro removido!"
                confirmation (novosLivros, novosRegistros)

adicionarUsuarioMenu :: [User] -> IO [User] -- DOCUMENTADO --
adicionarUsuarioMenu usuarios = do
    nome       <- inputString "Digite o nome do usuario: \n"
    matricula  <- input "Digite o número de matricula do usuário: \n" :: IO Int
    email      <- inputString "Digite o email do usuário: \n"

    let novo = User nome matricula email

    case adicionarusuario novo usuarios of
        Left erro -> do
            putStrLn erro
            confirmation usuarios
        Right novosUsuarios -> do
            putStrLn "Usuario cadastrado com sucesso!"
            confirmation novosUsuarios

listarUsuariosMenu :: [User] -> IO [User] -- DOCUMENTADO --
listarUsuariosMenu usuarios = do
    mapM_ (putStrLn . coutusuarios) usuarios
    confirmation usuarios

removerUsuarioMenu :: [User] -> [Livro] -> [Registro] -> IO ([User], [Livro], [Registro]) -- DOCUMENTADO --
removerUsuarioMenu usuarios livros registros = do
    idUsuario <- input "Digite o id do usuário a ser removido: " :: IO Int
    let usuarioParaRemover = filter (\l -> matricula l == idUsuario) usuarios
    case usuarioParaRemover of
        [] -> do
            putStrLn "Erro, usuário não encontrado"
            confirmation (usuarios, livros, registros)
        (_:_:_) -> do
            putStrLn "Multiplos usuarios!!"
            confirmation (usuarios, livros, registros)
        [usu] -> do
            case removerusuario idUsuario usuarios livros registros of
                Left erro -> do
                    putStrLn erro
                    confirmation (usuarios, livros, registros)
                Right (novoUsuarios, novoLivros, novoRegistros) -> do
                    putStrLn "Usuário removido!"
                    confirmation (novoUsuarios, novoLivros, novoRegistros)

registrarEmprestimoMenu :: [Livro] -> [User] -> [Registro] -> IO ([Livro], [Registro]) -- DOCUMENTADO --
registrarEmprestimoMenu livros usuarios registros = do
    putStrLn "ID do livro:"
    idlivro <- readLn
    putStrLn "Matrícula do usuário:"
    iduser <- readLn
    case find ((==iduser).matricula) usuarios of
        Nothing -> do
            putStrLn "Usuário não encontrado"
            _ <- getChar
            menuLivro livros usuarios registros
        Just user -> do
            resultado <- registraremprestimo idlivro user livros registros
            case resultado of
                Left erro -> do
                    putStrLn erro
                    _ <- getChar
                    menuLivro livros usuarios registros
                Right (novaLista, novosRegistros) -> do
                    putStrLn "Emprestimo registrado com sucesso!"
                    _ <- getChar
                    menuLivro novaLista usuarios novosRegistros



registrarDevolucoesMenu :: [Livro] -> [Registro] -> IO ([Livro], [Registro]) -- DOCUMENTADO --
registrarDevolucoesMenu livros registros = do
    id <- input "Digite o código do livro: \n"
    case registrarDevolucoes id livros registros of
        Left erro -> do
            putStrLn erro
            confirmation (livros, registros)
        Right novo -> do
            putStrLn "Devolução concluida!"
            confirmation novo

listarPorDisponibilidadeMenu :: [Livro] -> IO [Livro] -- DOCUMENTADO --
listarPorDisponibilidadeMenu livros = do
    status <- input "Digite o status Disponivel | Indisponivel | Emprestado\n"
    let listaFiltrada = listarPorDisponibilidade status livros
    mapM_ (putStrLn.coutlivro) listaFiltrada
    confirmation livros

exibirListaEsperaMenu :: [Livro] -> IO [Livro]
exibirListaEsperaMenu livros = do
    putStrLn "Digite o código do livro para exibir a lista de espera: "
    id <- input ""
    let filtraLivro = filter (\livro -> cod livro == id) livros
    case filtraLivro of
        [] -> do
            putStrLn "Livro não encontrado"
            confirmation livros
        (livro:_) -> do
            putStrLn (exibirlistaespera livro)
            confirmation livros

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
        newYear    <- input "Digite o novo ano do livro: \n" :: IO Int
        let newBook  = book {titulo = newTitle, autor = newAuthor, ano = newYear}
        let newBooks = map (\l -> if cod l == id then newBook else l) books
        putStrLn "Livro editado com sucesso!"
        confirmation newBooks

editarUsuarioMenu :: [User] -> [Livro] -> IO ([User], [Livro])
editarUsuarioMenu users books = do
    id <- input "Digite o id do usuário a ser editado: \n" :: IO Int
    let userLs = filter ((id ==) . matricula) users
    if length userLs /= 1 then do
        putStrLn "Erro, usuário não encontrado"
        confirmation (users, books)
    else do
        let user = head userLs
        putStrLn $ "Editando " ++ show user
        newName  <- inputString "Digite o novo nome do usuário: \n"
        newEmail <- inputString "Digite o novo email do usuário: \n"
        let newUser = user { nome = newName, email = newEmail }
        let newUsers = map (substUser newUser) users
        let newBooks = map (substDono user newUser) books
        let updatedBooks = map (substFila user newUser) newBooks
        putStrLn "Usuário editado com sucesso!"
        confirmation newUsers
        return (newUsers, updatedBooks)
    where
        substUser newUser u = if matricula u == matricula newUser then newUser else u
        substDono oldUser newUser l = if dono l == Just oldUser then l { dono = Just newUser } else l
        substFila oldUser newUser l = if oldUser `elem` fila l then l { fila = filter (/= oldUser) (fila l) ++ [newUser] } else l



-- Relatórios --

relatórioHistoricoMenu :: [User] -> [Registro] -> [Livro] -> IO ()
relatórioHistoricoMenu users registers livros = do
    userId <- input "Digite o numero de matricula do usuário: \n" :: IO Int
    let userLs = filter ((userId ==) . matricula) users
    case userLs of
        [] -> do
            putStrLn "Id inválido!!"
            confirmation ()
        (_:_:_) -> do
            putStrLn "Múltiplos Ids!!"
            confirmation ()
        [user] -> do
            let regs = filter ((== userId) . usuarioId) registers
                registrosComLivro = map (\r -> (r, buscaLivro (livroId r))) regs
                buscaLivro lid = head (filter ((== lid) . cod) livros)
            putStrLn $ "Histórico de " ++ nome user ++ ":"
            mapM_ printRegistro registrosComLivro
            confirmation ()
  where
    printRegistro (reg, livro) = putStrLn $
        "Livro: " ++ titulo livro ++
        " | Status: " ++ show (stat reg)

confirmation :: a -> IO a
confirmation x = do
    _ <- getLine
    return x