module Aula02 where
-- 2017 -> novo ano e nova atualização com base no livro de Haskell


dobro :: Int -> Int
dobro x = 2*x

-- O retorno eh sempre o ultimo a ser declarado
multi :: Double -> Double -> Double -> Double 
multi x y z = x*y*z

--Eh de bom tom que se coloque o cabeçalho

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n-1], mod b x == 0]


data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
            deriving Show


data Pergunta =  Sim | Nao
        deriving Show
        
MaiorQue :: Int -> Int -> Bool
MaiorQue x y = x > y

MenorQue :: Int -> Int -> Bool
MenorQue x y = x < y

Igual :: Int -> Int -> Bool
Igual x y =  (==) x y
