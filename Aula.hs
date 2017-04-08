import Data.Monoid

module Aula where

data Dia =  Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
            deriving Show
            
numDia :: Dia -> Int
numDia Domingo = 1
numDia Segunda = 2
numDia Terca = 3
numDia Quarta = 4
numDia Quinta = 5
numDia Sexta = 6
numDia Sabado = 7

diaBalada :: Dia -> Bool
diaBalada Sabado = True
diaBalada Domingo = True
diaBalada _ = False

valorHoraTrabalho :: Dia -> Double -> Double
valorHoraTrabalho Sabado x = 1.75*x
valorHoraTrabalho Domingo x = 2*x
valorHoraTrabalho _ x = x

data Day =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving Show
        
traduzirIP :: Day -> Dia
traduzirIP Monday = Segunda
traduzirIP Tuesday = Terca
traduzirIP Wednesday = Quarta
traduzirIP Thursday = Quinta
traduzirIP Friday = Sexta
traduzirIP Saturday = Sabado
traduzirIP Sunday = Domingo


traduzirPI :: Dia -> Day
traduzirPI Segunda = Monday 
traduzirPI Terca = Tuesday
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday
traduzirPI Domingo = Sunday

-- Exercicios Lista

-- 1.1

data Pergunta = Sim | Nao
        deriving Show
        
pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1

-- listPergs :: [Pergunta] -> [Int]
-- listPergs xs = 0
-- listPergs xs = 1

-- and1 :: Pergunta -> Pergunta -> Bool
-- and1 Sim Sim = True
-- and1 Nao _ = False


-- or1 :: Pergunta -> Pergunta -> Bool
-- or1 Sim _ = True
-- or1 _ Sim = True
-- or1 _ _ = False


-- 1.2

-- lista' :: [Int] -> [Int]
-- lista' 


-- 1.3

data Temperatura = Celsius | Farenheit | Kelvin
        deriving Show


converterCelsius :: Double -> Temperatura -> Double
converterCelsius x Farenheit = (x - 32) / 1.8
converterCelsius x Kelvin = x - 273.15

converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit x Celsius = x * 1.8 + 32
converterFarenheit x Kelvin = x + 273.15

converterKelvin :: Double -> Temperatura -> Double
converterKelvin x Celsius = x + 273.15
converterKelvin x Farenheit = ((x - 32) / 1.8) + 273.15

-- 1.4

data Joquempo = Pedra | Papel | Tesoura
        deriving Show

jogoJoquempo :: Joquempo -> Joquempo -> String
jogoJoquempo Pedra Papel = "Ganhou papel!"
jogoJoquempo Papel Pedra = "Ganhou papel!"
jogoJoquempo Pedra Tesoura = "Ganhou pedra!"
jogoJoquempo Tesoura Pedra = "Ganhou pedra!"
jogoJoquempo Papel Tesoura = "Ganhou tesoura!"
jogoJoquempo Tesoura Papel = "Ganhou tesoura!"
jogoJoquempo _ _ = "Empate!"



-- 1.5

-- 1.6

-- 1.7

data UnidadeImperial = Inch | Yard | Foot
        deriving Show
        
converterImperial :: Double -> UnidadeImperial -> Double
converterImperial x Inch = x + 0.0254
converterImperial x Yard = x + 0.9144
converterImperial x Foot = x + 0.3048


-- 1.8


data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
                deriving Show

data Hemisferio = Norte | Sul
                deriving Show

checaFim :: Mes -> Int
checaFim Janeiro = 31
checaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

-- 1.9

data Binario = Zero | Um
        deriving Show

--------------------- Aula 6 ------------------------------

-- Polimorfismo parametrico

data Objetos = Lapis | Oculos | Maquiagem | Celular | Agenda deriving Show


data Bolsa a b = Nada | UmaCoisa a | DuasCoisas a b
    deriving Show
    
--  a representa qualquer tipo, sendo que em caso de mais de um 'a' ele deve ter o mesmo tipo como parametro
-- isso serve para que não se faça preciso a criação de diversos data Bolsa para cada caso
--Aula> :t (DuasCoisas True False)
--(DuasCoisas True False) :: Bolsa Bool

-- o tipo Bolsa tem type parameter, o tipo Objetos não

-- quanto mais alto o nivel do kind (quanto mais parametros), mais complexo

-- o nome do campo extrai o campo da estrutura

-- data Exemplo a = {foo :: a} deriving Show

-- Aula> :t tail
-- tail :: [a] -> [a]
-- Aula> :t (+)
-- (+) :: Num a => a -> a -> a

-- typeclass são tipos que restringidos por estruturas
-- ex: Show Int, Eq Int, Num Int
-- typeclass = interface em orientação a objeto

data Curso = SI | ADS | GE |LOG | GP  deriving (Show, Eq, Ord)

-- deriving cria a instancia para poder exibir
-- Aula> ADS > SI
-- True

data Bolsas a = Bolso a | Mochila a a
-- o a deve ser restrito ao Eq
instance (Eq a) => Eq (Bolsas a) where
        (Bolso x) == (Bolso y) = x == y
        (Mochila x y) == (Mochila z w) = ((x == z) && (y == w)) || ((x == w) && (y == z))
        _ == _ = False
instance (Show  a) => Show (Bolsas a) where
        show (Bolso x) = "Um bolso com valor " ++ show x
        show (Mochila x y) = "Uma mochila com " ++ show x ++ " e " ++ show y
        
        
-- Aula> Bolso 1 == Bolso 2
-- False

-- criando a propria typeclass

data Resposta = Yes | No 
    deriving Show

class SimNao a where
    resp :: a -> Resposta
    
-- moneds e monoids 

-- Monedes : seja m =/= 0 um conjunto e <> uma operacao binaria. Uma monoide ( m, <>) eh uma estrutura que satisfazer as condicoes.
-- mappend e mepty
-- elemento que não faz nada e associatibilidade


-- conjunto, operação binario e elemento neutro
-- monoides só servem para coisas associativas, como adição e multiplicação


--class Monoid m where
-- mempty :: m
-- mappend :: m -> m -> m
-- mconcat :: [m] -> m
-- defining mconcat is optional, since it has the following default:
-- mconcat = foldr mappend mempty
 
-- this infix synonym for mappend is found in Data.Monoid
-- x <> y = mappend x y
-- infixr 6 <>
--together with the following laws: 
-- Identity laws
-- x <> mempty = x
--  <> x = x
-- Associativity
--(x <> y) <> z = x <> (y <> z)

-- as monoides facilitam as operacoes, facilitam os testes, o processamento. Teste orientado a propriedade.

--instance Monoid [a] where
--  mempty = []
--  mappend x y = x ++ y
--  mconcat = concat

-- exemplos de estanciamento de monoides 

--newtype Sum n = Sum n
 
--instance Num n => Monoid (Sum n) where
--  mempty = Sum 0
--  mappend (Sum x) (Sum y) = Sum (x + y)
 
--newtype Product n = Product n
 
--instance Num n => Monoid (Product n) where
--  mempty = Sum 1
--  mappend (Sum x) (Sum y) = Sum (x * y)

data Aluno = Aluno String (Sum Double)

instance Monoid Aluno where
        mempty = Aluno "Total: " (Sum 0)
        mappend (Aluno x n1) (Aluno y n2) = Aluno "Total: " (n1 <> n2)
        
data Bolsa2  a = Bolsa2 a | Vazio
        deriving Show

instance (Monoid a) => Monoid (Bolsa2 a) where
        mempty = Vazio
        mappend (Bolsa2 x) Vazio = Bolsa2 x
        mappend Vazio (Bolsa2 x) = Bolsa2 x
        mappend (Bolsa2 x) (Bolsa2 y) = Bolsa2 (x <> y)
        mappend Vazio Vazio = Vazio
        
-- Bolsa2 (Sum {getSum = 3})

data Binario = Zero | Um deriving Show


-- duas monoids para numero soma e multiplicação                                                                    
