module Tipos where

data Status = Disponivel | Indisponivel | Emprestado deriving(Show, Read, Eq)

data Livro = Livro
    { titulo    :: String
    , autor     :: String
    , ano       :: String
    , cod       :: Int
    , status    :: Status
    } deriving (Show, Read, Eq)

data User = User
    { nome      :: String
    , matricula :: Int
    , email     :: String
    } deriving (Show, Read, Eq)

-- ? type Fila = [User]
data Fila = Fila
    { usuarios :: [User]
    } deriving (Show, Read, Eq)

data Registro = Registro
    { usuarioId :: Int
    , livroId :: Int
    , stat :: Status
    } deriving (Show, Read, Eq)
