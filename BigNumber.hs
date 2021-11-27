module BigNumber where

-- 2.1. Definição do tipo BigNumber

type BigNumber = (Bool, [Int]) 

-- Por exemplo -12455 => (False, [1,2,4,5,5]) 
--                 21 => (True, [2,1])
--                  0 => (True, [0])

------------------------------------------------------------------------------------

-- Funções auxiliares


-- Remove os 0's à esquerda num BigNumber

removeLeadingZeros :: BigNumber -> BigNumber

removeLeadingZeros bigNum1@(signal1, x:xs) | (x == 0) && (length (x:xs) > 1) = removeLeadingZeros (signal1, xs)
                                           | otherwise = (signal1, x:xs)

-- Verifica se o valor absoluto do bigNum1 é maior que o valor absoluto do bigNum2. Retorna True se abs(bigNum1) >= abs(bigNum2), otherwise False.

checkBiggestNum :: BigNumber -> BigNumber-> Bool
checkBiggestNum bigNum1 bigNum2 = abs((read (output bigNum1) :: Int)) >= abs((read (output bigNum2) :: Int))


------------------------------------------------------------------------------------

-- 2.2. Converção de string para BigNumber

scanner :: String -> BigNumber

scanner "" = (True, [])

scanner str = removeLeadingZeros bigNum
        where
                bigNum | (head str) == '-' = (False, map (read . (:"")) (tail str) :: [Int])
                       | otherwise = (True , map (read . (:"")) str :: [Int])


------------------------------------------------------------------------------------

-- 2.3. Conversão de BigNumber para string
 
output :: BigNumber -> String 
output bigNum@(signal, bignum) | bignum == [] = ""
                               | signal = concat(map show bignum)
                               | signal == False = '-' : concat(map show bignum)


------------------------------------------------------------------------------------
                          
 
-- Aplic
sumBefore :: [Int] -> [Int] -> Int -> [Int]   
sumBefore [] list carry 
        | carry == 0 = list
        | otherwise = sumBefore [carry] list 0

sumBefore list [] carry
        | carry == 0 = list
        | otherwise = sumBefore list [carry] 0

sumBefore (x:xs) (y:ys) c = lastDigit : sumBefore xs ys rest
        where sumNum = x + y + c
              lastDigit = mod sumNum 10  -- Encontra o último dígito de sumNum, p.e: mod 15 10 = 5 
              rest = div sumNum 10       -- Encontra o resto da divisão inteira de sumNumb por 10, p.e: div 15 10 = 1


-- Aplic
subBefore :: [Int] -> [Int] -> Int -> [Int]   
subBefore [] list carry 
        | carry == 0 = list
        | otherwise = subBefore [carry] list 0

subBefore list [] carry
        | carry == 0 = list
        | otherwise = subBefore list [carry] 0

subBefore (x:xs) (y:ys) c = lastDigit : subBefore xs ys rest
        where sumNum = x - y - c
              lastDigit | sumNum < 0 = mod (10 - abs(sumNum)) 10  -- Encontra o último dígito de sumNum, p.e: mod 15 10 = 5 
                        | otherwise = sumNum
              rest | sumNum < 0 =  1      -- Encontra o resto da divisão inteira de sumNumb por 10, p.e: div 15 10 = 1
                   | otherwise = 0


somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) |  (signal1 /= signal2) && checkBiggestNum bigNum1 bigNum2 = removeLeadingZeros ((signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0)))
                                                             |  (signal1 /= signal2) && checkBiggestNum bigNum2 bigNum1 = removeLeadingZeros ((signal2, reverse(subBefore (reverse bignum2) (reverse bignum1) 0)))
                                                             |  otherwise = removeLeadingZeros ((signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0)))



subBN :: BigNumber -> BigNumber -> BigNumber
subBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2)  |  (signal1 /= not signal2) && (checkBiggestNum bigNum1 bigNum2) = removeLeadingZeros ((signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0)))
                                                             |  (signal1 /= not signal2) && (checkBiggestNum bigNum2 bigNum1) = removeLeadingZeros ((signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0)))
                                                             |  otherwise = removeLeadingZeros ((signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0)))


------------------------------------------------------------------------------------

-- 2.6. Multiplicação de BigNumbers

-- Função auxiliar que multiplica 2 BigNumbers

mulAux :: BigNumber -> BigNumber -> BigNumber

mulAux nb1@(_, []) nb2@(_,_) = (True, [0])                 

mulAux nb1@(signal1, x:xs) nb2@(signal2, ys) = somaBN (True, (map (x*) ys)) (mulAux (True,xs) (True, (ys++ [0])))


-- Função principal que chama a função "mulAux" e calcula o sinal do resultado

mulBN :: BigNumber -> BigNumber -> BigNumber

mulBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) = bigN3
        where
                ret@(signal3, bignum3) = mulAux (True, reverse bignum1) (True, bignum2)
                bigN3 = (signal1 == signal2, bignum3)


------------------------------------------------------------------------------------

-- 2.7. Divisão de BigNumbers

-- Função auxiliar que multiplica 2 BigNumbers

divAux :: BigNumber -> BigNumber -> BigNumber

divAux bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) | (checkBiggestNum bigNum1 bigNum2) = somaBN (True, [1]) (divAux (subBN bigNum1 bigNum2) bigNum2)
                                                             | otherwise = (True, [0])

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)

-- Função principal que chama a função "divAux" e calcula o sinal do resultado

divBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) = (quociente, resto)
        where
                ret@(signal3, bignum3) = divAux (True, bignum1) (True, bignum2)
                quociente = (signal1 == signal2, bignum3)
                resto = subBN bigNum1 (mulBN quociente bigNum2)


------------------------------------------------------------------------------------

-- 5. Divisão de BigNumbers segura


-- Função que calcula a divisão entre 2 BigNumbers e impede divisões por 0 em compile-time

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)

safeDivBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) | ((output bigNum2) == "0") = Nothing
                                                                | otherwise = Just (divBN bigNum1 bigNum2)




