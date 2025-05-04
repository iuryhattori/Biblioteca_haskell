module Funcoes where
import Tipos


AdicionarLivro:: Livro -> [Livro] -> Either String [Livro]
AdicionarLivro liv x =  if elem liv x
                        then Left "Erro! Livro já registrado"
                        else Right (liv : x)


RemoverLivro :: Livro -> [Livro] -> Either String [Livro]
RemoverLivro liv x = if elem liv x
                     then Right (filter (\p -> p /= liv) x) 
                     else Left "Erro! Livro não registrado!"


AdicionarUsuario :: User -> [User] -> Either String [User]
AdicionarUsuario us x = if elem us x
                        then Left ("Erro! Usuário já cadastrado)
                        else Right (us : x)

RemoverUsuario :: User -> [User] -> Either String [Livro]
RemoverUsuario us x =   if elem us x
                        then Right (filter (\p -> p /= var) x)
                        else Left "Erro! Usuário não cadastrado"

RegistrarEmprestimo :: String -> [Livro] -> Either String [Livro]
RegistrarEmprestimo t x =   if elem t (map titulo x) -- aplicar map para puxar os titulos dos livros
                            then Right (map (\p -> if titulo p == t then p {status = Emprestado} else p )x )
                            else Left "Erro! Livro não registrado"

RegistrarDevolucoes :: String -> [Livro] -> Either String [Livro]
RegistrarDevolucoes t x =   if elem t (map titulo x)
                            then Right (map (\p -> if titulo p == t then p {status = Disponivel} else p) x)
                            else Left "Erro! Livro não registrado"
-- Testado!!
ListaEspera :: User -> Fila -> Either String Fila
ListaEspera user queue =    if elem user (usuarios queue)
                            then Left "Erro! Usuário já está na fila"
                            else Right queue { usuarios = user : usuarios queue}

