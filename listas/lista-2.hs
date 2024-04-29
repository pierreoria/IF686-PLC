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


