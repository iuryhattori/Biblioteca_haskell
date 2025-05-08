module Tipos where

data Status = Disponivel | Indisponivel | Emprestado deriving(Show, Read, Eq)

data Livro = Livro
    { titulo    :: String
    , autor     :: String
    , ano       :: String
    , cod       :: Int
    , status    :: Status
    , dono      :: Maybe User
    } deriving (Show, Read, Eq)

data User = User
    { nome      :: String
    , matricula :: Int
    , email     :: String
    } deriving (Show, Read, Eq)

data Fila = Fila
    {
        usuarios :: [User]
    }


   
