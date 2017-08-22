module Aula03 where
-- aulas 02 e 03

-- notação matemática em funções haskell
-- nome da expressão -> parametro -> expressão
triplo :: Int -> Int
triplo x = 3*x

-- nome da expressão -> parametro -> parametro -> expressão (retorno)
-- tipo da expressão somar = Int -> Int -> Int
somar :: Int -> Int -> Int
somar x y = x+y

-- retorna o tipo de f 
f :: String -> Int -> Int
f ls x = x + length ls

-- data constructor (tdo tipo no Haskell é obrigatório ser em letra maiúscula. Todo tipo tem um conjunto de valores)
-- no caso abaixo, o tipo Dia possui os valores de Segunda a Domingo
-- type safe = o compilador haskell já faz o controle
-- tipo soma ou some type 
-- Segunda é um value constructor agora, por exemplo
-- Tipo somatorio
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
            deriving (Eq , Show, Enum)
 
 -- a comparação já é feita antes da função começar, o que poupa código e procesamento   
 -- o _ é o valor defalt da função 
agenda :: Dia -> String
agenda  Sabado = "Cinema" 
agenda  Domingo = "Dormir"
agenda  _ = "Trabalho e FATEC" 


-- EXERCICIOS:

-- 01) Faça uma função que converta os dias para Ints. Segunda vale 1, Terca 2 e assim por diante.

numDias :: Dia -> Int
numDias Segunda = 1
numDias Terca = 2
numDias Quarta = 3
numDias Quinta = 4
numDias Sexta = 5
numDias Sabado = 6
numDias Domingo = 7

-- 02) Crie um tipo chamado Day contendo os dias da semana em inglês. Implemente a função traduzirPI
-- que recebe um dia em portugues e o traduz para ingles

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq,Show, Enum)
        
traduzirPI :: Dia -> Day
traduzirPI Segunda = Monday
traduzirPI Terca = Tuesday
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday
traduzirPI Domingo = Sunday


-- 03) Faça o mesmo que pede no enunciado do exercicio 1 para o tipo Day

numDays :: Day -> Int
numDays Monday = 1
numDays Tuesday = 2 
numDays Wednesday = 3 
numDays Thursday = 4
numDays Friday = 5 
numDays Saturday = 6
numDays Sunday = 7

-- 04) Faça a função traduzirIP e traduza os dias da semana de ingles para portugues
traduzirIP :: Day -> Dia
traduzirIP Monday =  Segunda 
traduzirIP Tuesday = Terca
traduzirIP Wednesday = Quarta
traduzirIP Thursday = Quinta
traduzirIP Friday = Sexta
traduzirIP Saturday = Sabado
traduzirIP Sunday = Domingo

-- Exclui os impares e só retorna os pares (even = par, odd = impar)
-- *Aula03> [x | x <- [Segunda .. Domingo], even (numDias x)]
-- [Terca,Quinta,Sabado]
-- *Aula03> 
       
-- PODEMOS TER UM VALUE CONSTRUCTOR COM OS MESMO NOME DO TIPO
-- Um value cosntructor pode conter campos (no caso abaixo, dois. Exemplo: nome e idade)
-- Aqui não pode ter Enum, ele é um tipo multiplicativo
data Pessoa = Pessoa String Int
    deriving (Show, Eq)
    
aniversario :: Pessoa -> Pessoa
aniversario (Pessoa nome idade) = Pessoa nome (idade+1)

-- *Aula03> aniversario (Pessoa "Flavia" 24)
-- Pessoa "Flavia" 25

data Naipe = Ouros | Paus | Copas | Espadas
        deriving (Show, Eq, Enum)
        
data ValorCartas = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Dama | Valete | Rei
        deriving (Show, Eq, Enum)
        
data  Carta = Carta ValorCartas Naipe
        deriving (Show,Eq)

-- 05) Faça uma função que retorne a cor das cartas(vermelho ou preto)
data Cor = Preto | Vermelho
        deriving Show
        
corCartas :: Carta -> Cor
corCartas (Carta numero Ouros) = Vermelho
corCartas (Carta numero Copas) = Vermelho
corCartas _ = Preto

