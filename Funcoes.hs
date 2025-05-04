module Funcoes where
import Tipos


adicionarlivro:: Livro -> [Livro] -> Either String [Livro]
adicionarlivro liv x =  if elem liv x
                        then Left "Erro! Livro já registrado"
                        else Right (liv : x)

listarlivros :: Livro -> String
listarlivros mostrarlivros =
    "Título: " ++ (titulo mostrarlivros) ++ "; " ++ 
    "Autor: " ++ (autor mostrarlivros) ++ "; " ++
    "Ano: " ++ show (ano mostrarlivros) ++ "; " ++
    "Código Único: " ++ show (cod mostrarlivros) ++ "; " ++
    "Status: " ++ show (status mostrarlivros)

removerLivro :: Livro -> [Livro] -> Either String [Livro]
removerLivro liv x = if elem liv x
                     then Right (filter (\p -> p /= liv) x) 
                     else Left "Erro! Livro não registrado!"


adicionarusuario :: User -> [User] -> Either String [User]
adicionarusuario us x = if elem us x
                        then Left "Erro! Usuário já cadastrado"
                        else Right (us : x)

listarusuarios :: User -> String
listarusuarios mostrarUser =
    "Nome: " ++ show (nome mostrarUser) ++ "; " ++ 
    "Matrícula: " ++ show ( matricula mostrarUser) ++ "; " ++
    "Email: " ++ show (email mostrarUser) 

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

