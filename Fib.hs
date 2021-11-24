--Cálculo recursivo do n elemento de uma sequência de fiibonacci

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec number = fibRec(number - 1) + fibRec(number - 2)  




fibRec :: (Integral a) => a -> a
fibRec 0 = 0 : []
fibRec 1 = 1  
fibRec number = fibRec(number - 1) + fibRec(number - 2)





-- Cálculo de uma lista infinita de fibonacci numbers

fibInf :: (Integral a) => [a]
fibInf = 0 : 1 : zipWith (+) fibInf (tail fibInf)


-- Cálculo do elemento de índice n da lista 

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = fibInf !! (fromIntegral n)
