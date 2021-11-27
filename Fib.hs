import BigNumber

-- 1.1. Cálculo recursivo do n elemento de uma sequência de fibonacci

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec number = fibRec(number - 1) + fibRec(number - 2)  

------------------------------------------------------------------------------------

-- 3.1. Cálculo recursivo do n elemento de uma sequência de fibonacci usando BigNumbers

fibRecBN :: BigNumber -> BigNumber 
fibRecBN bigNum@(True, [0]) = (True, [0])
fibRecBN bigNum@(True, [1]) = (True, [1])
fibRecBN bigNum@(True, bignum) = somaBN (fibRecBN(subBN bigNum (True, [1]))) (fibRecBN(subBN bigNum (True, [2]))) 


------------------------------------------------------------------------------------

-- 1.2. Cálculo de uma lista de fibonacci numbers enquanto o comprimento da lista não for maior do que o índice do elemento que queremos calcular ('a')

listCreate :: (Integral a) => a -> [a]
listCreate a = until (\x -> length x > (fromIntegral a)) (\x -> x ++ [last x + last (init x)]) [0,1]

-- [0,1] -> [0,1] ++ [1+0] -> [0,1,1] ...


-- Cálculo do enésimo número de fibonacci

fibLista :: (Integral a) => a -> a
fibLista a = listCreate a !! (fromIntegral a) 

------------------------------------------------------------------------------------

-- 3.2. Cálculo do enésimo número de fibonacci usando uma lista de resultados parciais e BigNumbers

listCreateBN :: BigNumber -> [BigNumber]
listCreateBN bigNum = until (\x -> length x > (read (output bigNum) :: Int)) (\x -> x ++ [somaBN (last x) (last (init x))]) [(True,[0]),(True, [1])]


-- Cálculo do enésimo número de fibonacci usando BigNumbers

fibListaBN :: BigNumber -> BigNumber
fibListaBN bigNum = listCreateBN bigNum !! (read (output bigNum) :: Int)



------------------------------------------------------------------------------------

-- 1.3. Cálculo de uma lista infinita de fibonacci numbers

fibInf :: (Integral a) => [a]
fibInf = 0 : 1 : zipWith (+) fibInf (tail fibInf)

-- Cálculo do elemento de índice n da lista infinit a de fibonacci numbers

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = fibInf !! (fromIntegral n)

------------------------------------------------------------------------------------


-- 3.3. Cálculo de uma lista infinita de fibonacci numbers usando BigNumbers

fibInfBN :: [BigNumber]
fibInfBN = (True, [0]) : (True, [1]) : zipWith (somaBN) fibInfBN (tail fibInfBN)

-- Cálculo do elemento de índice n da lista infinita de fibonacci numbers usando Big Numbers

fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN bigNum = fibInfBN !! (read (output bigNum) :: Int)