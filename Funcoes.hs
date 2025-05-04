module Funcoes where
import Tipos


adicionarlivro:: Livro -> [Livro] -> Either String [Livro]
adicionarlivro liv x =  if elem liv x
                        then Left "Erro! Livro já registrado"
                        else Right (liv : x)

coutlivro :: Livro -> String -- função que lista apenas 1 livro, a intenção é usa-la como função auxiliar
coutlivro mostrarlivro =
    "Título: " ++ (titulo mostrarlivro) ++ "; " ++ 
    "Autor: " ++ (autor mostrarlivro) ++ "; " ++
    "Ano: " ++ show (ano mostrarlivro) ++ "; " ++
    "Código Único: " ++ show (cod mostrarlivro) ++ "; " ++
    "Status: " ++ show (status mostrarlivro)

listarlivros :: [Livro] -> String
listarlivros livros = unlines (map coutlivro livros)

removerLivro :: Livro -> [Livro] -> Either String [Livro]
removerLivro liv x = if elem liv x
                     then Right (filter (\p -> p /= liv) x) 
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

listarusuarios :: [User] -> String
listarusuarios users = unlines (map coutusuarios users)

removerusuario :: User -> [User] -> Either String [User]
removerusuario us x =   if elem us x
                        then Right (filter (\p -> p /= us) x)
                        else Left "Erro! Usuário não cadastrado"

registraremprestimo :: String -> [Livro] -> Either String [Livro]
registraremprestimo t x =   if elem t (map titulo x) -- aplicar map para puxar os titulos dos livros
                            then Right (map (\p -> if titulo p == t then p {status = Emprestado} else p )x )
                            else Left "Erro! Livro não registrado"

registrardevolucoes :: String -> [Livro] -> Either String [Livro]
registrardevolucoes t x =   if elem t (map titulo x)
                            then Right (map (\p -> if titulo p == t then p {status = Disponivel} else p) x)
                            else Left "Erro! Livro não registrado"
-- Testado!!
listaespera :: User -> Fila -> Either String Fila
listaespera user queue =    if elem user (usuarios queue)
                            then Left "Erro! Usuário já está na fila"
                            else Right queue { usuarios = user : usuarios queue}


exibirlistaespera :: Fila -> String
exibirlistaespera fil =
    unlines (map coutusuarios (usuarios fil)) ++
    "\nTotal de usuários fila: " ++ show (length (usuarios fil))

