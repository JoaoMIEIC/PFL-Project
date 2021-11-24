--Cálculo recursivo do n elemento de uma sequência de fiibonacci

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec number = fibRec(number - 1) + fibRec(number - 2)  


------------------------------------------------------------------------------------
-- Cálculo de uma lista de fibonacci numbers enquanto o comprimento da lista não for maior do que o índice do elementoo que queremos calcular ('a')

listCreate :: (Integral a) => a -> [a]
listCreate a = until (\x -> length x > (fromIntegral a)) (\x -> x ++ [last x + last (init x)]) [0,1]

-- [0,1] -> [0,1] ++ [1+0] -> [0,1,1] ...

fibLista :: (Integral a) => a -> a
fibLista a = listCreate a !! (fromIntegral a) 

-- Cálculo de uma lista infinita de fibonacci numbers

fibInf :: (Integral a) => [a]
fibInf = 0 : 1 : zipWith (+) fibInf (tail fibInf)


------------------------------------------------------------------------------------
-- Cálculo do elemento de índice n da lista infinit a de fibonacci numbers

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = fibInf !! (fromIntegral n)
