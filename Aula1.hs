module Aula1 where

-- juudehuef
{-
ojkejiegioguioghuoeg
-}
ehPrimo :: Int -> Bool
ehPrimo n = length ([x | x <- [2 .. (n-1)], mod n x == 0]) == 0

lista :: [Int]
lista = [x | x <- [2 .. 100], ehPrimo x]

data Cor = Azul | Vermelho | Verde | Preto |
           Branco deriving Show
           
converter :: Cor -> (Int, Int, Int)
converter Azul = (0, 0, 255)
converter Verde = (0, 255, 0)
converter Vermelho = (255, 0, 0)
converter Preto = (0, 0, 0)
converter Branco = (255, 255, 255)

misturar :: (Cor, Cor) -> Cor
misturar (Azul, Vermelho) = Verde
misturar (_, Preto) = Preto
misturar (Preto, _) = Preto
misturar (x, Branco) = x
misturar (Branco, x) = x
misturar (_, _) = Azul



somar1 :: Int -> Int -> Int
somar1 x y = x+y

somar2 :: (Int, Int) -> Int
somar2 (x, y) = x+y

trdInt :: (Int, Int, Int) -> Int
trdInt (_, _, z) = z 

-- RECORD SYNTAX: Eh possivel dar nomes aos
-- campos dos VC. Esses nomes sao funcoes de projecao
-- (GET)

data Correncia = Euro | Real |  Dolar  deriving Show

data Dinheiro = Dinheiro {valorDinheiro :: Double, 
                          currDinheiro :: Correncia} 
                          deriving Show

data Sexo = M | F deriving Show

data Pessoa = Pessoa {nomePessoa :: String,
                      sexoPessoa :: Sexo,
                      dinPessoa :: Dinheiro} 
                      deriving Show

-- Em uma empresa Brasileira, um bonus mensal
-- foi dado aos funcionarios. As mulheres gnharam
-- 1500 reais e os homens 800. Nos Eua,
-- mulheres e homens ganham 600 Dolares a mais.
-- Na Europa nao ha bonus.

bonus :: Pessoa -> Pessoa
bonus (Pessoa n F (Dinheiro valor Real)) = Pessoa n F (Dinheiro (valor+1500) Real)
bonus (Pessoa n M (Dinheiro valor Real)) = Pessoa n M (Dinheiro (valor+800) Real)
bonus (Pessoa n s (Dinheiro valor Dolar)) = Pessoa n s (Dinheiro (valor+600) Dolar)
bonus x = x

-- EXEMPLO> Quero fazer uma funcao que some dois
-- Dinheiro

conversaoDolar :: Dinheiro -> Dinheiro
conversaoDolar (Dinheiro valor Real) = Dinheiro (0.312437*valor) Dolar
conversaoDolar (Dinheiro valor Euro) = Dinheiro (1.13*valor) Dolar
conversaoDolar x = x

conversaoEuro :: Dinheiro -> Dinheiro
conversaoEuro (Dinheiro valor Real) = Dinheiro (0.275992226*valor) Euro
conversaoEuro (Dinheiro valor Dolar) = Dinheiro (0.88*valor) Euro
conversaoEuro x = x

conversaoReal :: Dinheiro -> Dinheiro
conversaoReal (Dinheiro valor Euro) = Dinheiro (3.62*valor) Real
conversaoReal (Dinheiro valor Dolar) = Dinheiro (3.20*valor) Real
conversaoReal x = x

somarDinheiro :: Dinheiro -> Dinheiro -> Dinheiro
somarDinheiro (Dinheiro v1 Real) v2 = Dinheiro (v1 + valorDinheiro (conversaoReal v2)) Real
somarDinheiro (Dinheiro v1 Dolar) v2 = Dinheiro (v1 + valorDinheiro (conversaoDolar v2)) Dolar
somarDinheiro (Dinheiro v1 Euro) v2 = Dinheiro (v1 + valorDinheiro (conversaoEuro v2)) Euro

{-*

HIGH-ORDER FUNCTIONS: Em Haskell as funcoes
sao tratadas como valores comuns. Ou seja,
sao passadas via parametro ou retornadas.

CURRYING: Eh o ato de chamar funcao
com menos parametros do que definido. O resultado
disso eh uma funcao com os parametros restantes.

*-}
somar :: Int -> (Int -> Int)
somar x = \y -> x+y

dobro :: Int -> Int
dobro x = 2*x

func :: (Int -> Int) -> Int
func f = 2 + f 10

somarTres :: Int -> Int -> Int -> Int
somarTres x y z = x+y+z

elimNeg :: [Int] -> [Int]
elimNeg xs = filter (\x -> x>=0) xs


-- HIGH-ORDER FUNCTIONS: Em Haskell as funcoes
-- sao tratadas como valores comuns. Ou seja,
-- sao passadas via parametro ou retornadas.

-- CURRYING: Eh o ato de chamar funcao
--com menos parametros do que definido. O resultado
--disso eh uma funcao com os parametros restantes.

-- EXPRESSAO LAMBDA: FUNCAO SEM CORPO.
-- REPRESENTA UM VALOR DO TIPO FUNCAO

dobro :: Int -> Int
dobro x = 2*x

aplicar :: (Int -> Int) -> Int
aplicar f = 1 + f 10

aplicar2 :: (Int -> Int -> Int) -> Int
aplicar2 f = 1 + f 10 20

somarTres :: Int -> Int -> Int -> Int
somarTres x y z = x+y+z

tamanho :: String -> Int
tamanho x = length x

rabo :: String -> String
rabo x = tail x

data Pokemon = Charmander | Squirtle | Bulbassaur 
               deriving Show

data Trainer = Trainer String Pokemon deriving Show

vencedor :: Trainer -> Trainer -> String
vencedor (Trainer n1 Squirtle) (Trainer n2 Charmander) = n1
vencedor (Trainer n1 Charmander) (Trainer n2 Squirtle) = n2
vencedor (Trainer n1 Bulbassaur) (Trainer n2 Squirtle) = n1
vencedor (Trainer n1 Squirtle) (Trainer n2 Bulbassaur) = n2
vencedor (Trainer n1 Bulbassaur) (Trainer n2 Charmander) = n2
vencedor (Trainer n1 Charmander) (Trainer n2 Bulbassaur) = n1
vencedor _ _ = "Empate"
