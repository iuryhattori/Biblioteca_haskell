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
    registrardevolucoes,
    listarPorDisponibilidade
) where
import Tipos


adicionarlivro:: Livro -> [Livro] -> Either String [Livro]
adicionarlivro liv x =  if elem liv x
                        then Left "Erro! Livro já registrado"
                        else Right (liv : x)

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



removerLivro :: Int -> [Livro] -> Either String [Livro]
removerLivro id livros = 
    if any (\t -> cod t == id) livros
        then Right (filter (\p -> cod p /= id) livros) 
        else Left "Erro! Livro não registrado!"


adicionarusuario :: User -> [User] -> Either String [User]
adicionarusuario us x = if elem us x
                        then Left "Erro! Usuário já cadastrado"
                        else Right (us : x)

coutusuarios :: User -> String
coutusuarios mostrarUser =
    "Nome: " ++ show (nome mostrarUser) ++ "; " ++ 
    "Matrícula: " ++ show ( matricula mostrarUser) ++ "; " ++
    "Email: " ++ show (email mostrarUser) 

removerusuario :: Int -> [User] -> Either String [User]
removerusuario id usuarios =
    if any (\t -> matricula t == id) usuarios
        then Right (filter (\t -> matricula t /= id) usuarios)
        else Left "Erro! Usuario não registrado!"

registraremprestimo :: Int -> User -> [Livro] -> IO (Either String [Livro])
registraremprestimo id user livros =
    case break (\l -> cod l == id) livros of
        (_, []) -> return $ Left "Erro: livro não encontrado"
        (antes, livro:depois) ->
            case status livro of
                Disponivel -> do
                    let livroEmprestado = livro {status = Emprestado, dono = Just user}
                    return $ Right (antes ++ [livroEmprestado] ++ depois)
                Emprestado -> do
                    putStrLn "Livro indisponível, gostaria de entrar na lista de espera? sim/não"
                    resposta <- getLine
                    if resposta == "sim" then
                        if user `elem` fila livro then
                            return $ Left "Você já está na fila deste livro!"
                        else do
                            let novaFila = fila livro ++ [user]
                                livroAtualizado = livro {fila = novaFila}
                            return $ Right (antes ++ [livroAtualizado] ++ depois)
                    else
                        return $ Left "Ok!"
                Indisponivel -> return $ Left "Livro está indisponível"

registrardevolucoes :: Int -> [Livro] -> Either String [Livro]
registrardevolucoes t livros =
    if elem t (map cod livros)
    then Right (map (\livro -> if cod livro == t then livro {status = Disponivel, dono = Nothing} else livro) livros)
    else Left "Erro, livro não encontrado"

listarPorDisponibilidade :: Status -> [Livro] -> [Livro]
listarPorDisponibilidade sta lista = filter (\t -> status t == sta) lista

-- Testado!!
listaespera :: User -> Fila -> Either String Fila
listaespera user queue =    if elem user (usuarios queue)
                            then Left "Erro! Usuário já está na fila"
                            else Right queue { usuarios = user : usuarios queue}

exibirlistaespera :: Livro -> String
exibirlistaespera livro =
    unlines (map coutusuarios (fila livro)) ++ "\nTotal de usuários na fia: " ++ show (length(fila livro))
