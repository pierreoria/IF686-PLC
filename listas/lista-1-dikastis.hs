{-
Defina uma função expNE que calcule o resultado da exponenciação inteira de X elevado à Y, sem recorrer a funções pré-definidas.
-}

expNE :: Float -> Int -> Float
expNE f 0 = 1
expNE f i = f * expNE f (i-1)

{-
Defina uma função que retorna todos os números quadrados perfeitos <= n, em ordem decrescente
-}

perfeitos :: Integer -> [Integer]
perfeitos 1 = [1]
perfeitos n = helper (floor (sqrt (fromIntegral n))) n

helper :: Integer -> Integer -> [Integer]
helper 0 n = []
helper i n
  | ok i n = [i*i] ++ helper (i-1) n
  | otherwise = helper (i-1) n

ok :: Integer -> Integer -> Bool
ok i n 
  | (i*i) <= n = True
  | otherwise = False

{-
Defina função:
numDiv:: Integral a => a −> a −> a
Que recebe dois valores inteiros e retorna o número de vezes que uma divisão exata pode ser realizada
-}

numDiv:: Integral a => a -> a -> a
numDiv a b
  | (a `mod` b) /= 0 = 0
  | otherwise = 1 + numDiv (a `div` b) b


{-
Defina uma função recursiva para calcular o máximo divisor comum de dois números inteiros não negativos a e b, usando o algoritmo de Euclides.
-}

mdc:: Integral a => a -> a -> a
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

{-
Defina uma função que retorna os elementos de 1..n tal que não são divisores exatos de qualquer dos elementos posteriores a eles na lista.
-}

primosN :: Int -> [Int]
primosN n = [ceiling (fromIntegral n / 2)..n]

{-
Defina as seguintes funções, que retornam o máximo de quatro inteiros:
maxFour :: Integer −> Integer −> Integer −> Integer −> Integer -- descrita com base em maxThree;
maxFour' :: Integer −> Integer −> Integer −> Integer −> Integer -- descrita com base em max;
maxFour'' :: Integer −> Integer −> Integer −> Integer −> Integer -- descrita com base em max e maxThree;
-}

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer 
maxFour a b c d = maxThree a b (maxThree b c d)

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer 
maxFour' a b c d = max (max a b ) (max c d) 

maxFour'' :: Integer -> Integer -> Integer -> Integer -> Integer 
maxFour'' a b c d = max a (maxThree b c d) 

maxThree:: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c


{-
Defina uma função recursiva:
merge :: Ord a => [a] -> [a] -> [a]
Que une duas listas ordenadas e resulta em uma única lista ordenada.
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs)
  | a < b = a : merge as (b:bs)
  | otherwise = b : merge (a:as)  bs



