import Data.Bits

dobro :: Int -> Int
dobro a = a + a

quad :: Int -> Int
quad a = dobro a + dobro a

poli2 :: Int -> Int -> Int -> Int -> Int
poli2 a b c x = (a * x * x) + (b * x) + c

parImpar :: Int -> String
parImpar x
    | x .&. 1 == 1 = "Impar"
    | otherwise = "Par"

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c
    | a > b && a > c = a
    | b > c = b
    | otherwise = c

maxFour1 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour1 a b c d = 
    let k = maxThree a b c
    in if d > k
        then d
    else k

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = max (max a b) (max c d)

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d = max a (maxThree b c d)


quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais a b c 
    | a == b && b == c = 3
    | a == b || a == c = 2
    | b == c = 2
    | otherwise = 0

{-
    7:
  
    Usando casamento de padrão, definir a função ehZero que retorna verdadeiro se for
    dado como argumento um inteiro que seja 0, e falso, caso contrário. Definir o tipo da
    função ehZero
-}

sumTo :: Integer -> Integer
sumTo n
    | n == 1 = 1
    | otherwise = n + sumTo (n-1)

potencia :: Integer -> Integer -> Integer
potencia n k
    | k == 0 = 1
    | otherwise = n * potencia (n) (k-1)

{-

    Usando recursão, compute os coeficientes binomiais dados pelas seguintes equações:
        B(n, k) = B(n − 1, k) + B(n − 1, k − 1)
        B(n, 0) = 1
        B(0, k) = 0, quando k > 0
    Dica: usar casamento de padrão pode ser de grande ajuda.

-}

bin :: (Int , Int) -> Int
bin (n,k)
    | n == 0 && k > 0 = 0
    | k == 0 = 1
    | otherwise = bin ((n-1),k) + bin((n-1),(k-1))


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fib' (n-2) 0 1
  where
    fib' 0 x y = x + y
    fib' i x y = fib' (i-1) y (x+y)

{-
11. Os números de Tribonacci são dados pelas seguintes equações
T(1) = 1
T(2) = 1
T(3) = 2
T(n + 1) = T(n) + T(n − 1) + T(n − 2)

Implemente uma função recursiva eficiente que calcula T n. Considere o uso de uma
função auxiliar.
-}

trib :: Int -> Integer
trib n = trib' n (0, 0, 1)
  where
    trib' 0 (a, _, _) = a
    trib' 1 (_, b, _) = b
    trib' 2 (_, _, c) = c
    trib' n (a, b, c) = trib' (n - 1) (b, c, a + b + c)

addEspacos :: Int -> String
addEspacos n = aux' n ""
    where
        aux' 0 x = x
        aux' n x = aux' (n-1) (x ++ " ")

{-
    Defina a função paraDireita utilizando a definição de addEspacos para adicionar uma
    quantidade n de espaços à esquerda de um dado String, movendo o mesmo para a direita.
    paraDireita :: Int −> String −> String
-}

paraDireita :: Int -> String -> String
paraDireita n s = aux' n s
    where
        aux' 0 s = s
        aux' n s = aux' (n-1) (" " ++ s) 

{-

    Escreva uma função para retornar, em forma de tabela, todas as vendas da semana 0 até
    a semana n, incluindo o total e a média de vendas no período. Usem as funções definidas
    previamente e defina novas funções que achar necessário.
    Semana Venda
    0 12
    1 14
    2 15
    Total 41
    Média 13.6667


-}
