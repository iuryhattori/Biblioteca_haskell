module Tipos where

data Status = Disponivel | Indisponivel | Emprestado deriving(Show, Read, Eq)
data StatusRg = Esperando | Atual | Passado deriving(Show, Read, Eq)

data Livro = Livro
    { titulo    :: String
    , autor     :: String
    , ano       :: Int
    , cod       :: Int
    , status    :: Status
    , dono      :: Maybe User
    , fila      :: [User]
    } deriving (Show, Read, Eq)

data User = User
    { nome      :: String
    , matricula :: Int
    , email     :: String
    } deriving (Show, Read, Eq)


data Fila = Fila {
    usuarios :: [User]  
}



data Registro = Registro
    { usuarioId :: Int
    , livroId :: Int
    , stat :: StatusRg
    } deriving (Show, Read, Eq)
