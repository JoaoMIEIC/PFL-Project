-- Definição do tipo BigNumber

type BigNumber = (Bool, [Int]) 

-- Por exemplo -12455 => (True, [-1,2,4,5,5]) 

-- Aplica a função read após aplicar a função (:"") que torna cada char numa string, a cada um dos elementos da string str (lista de chars) 
-- criando listas individuais de cada um dos números e depois aplica map para criar uma lista de todos os dígitos de str.

scanner :: String -> BigNumber
scanner str | (head str) == '-' = (True, map (read . (:"")) (tail str) :: [Int])
            | otherwise = (False , map (read . (:"")) str :: [Int])


-- Aplica a função show que dá preapend a todos os elementos de bigNum através da função map
 
output :: BigNumber -> String 
output bigNum@(signal, bignum) | signal = '-' : concat(map show bignum)
                               | signal == False = concat(map show bignum)




checkBiggestNum :: BigNumber -> BigNumber-> Bool
checkBiggestNum bigNum1 bigNum2 | (read (output bigNum1) :: Int) > (read (output bigNum2) :: Int) = True
                          | otherwise = False
                          
 
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
              lastDigit = mod (10 - abs(sumNum)) 10  -- Encontra o último dígito de sumNum, p.e: mod 15 10 = 5 
              rest | sumNum < 0 =  1      -- Encontra o resto da divisão inteira de sumNumb por 10, p.e: div 15 10 = 1
                   | otherwise = 0


somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) |  signal1 && signal2 = (signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0))
                                                             |  (signal1 /= signal2) && length bignum1 > length bignum2 = (signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0))
                                                             |  (signal1 /= signal2) && length bignum2 > length bignum1 = (signal2, reverse(subBefore (reverse bignum2) (reverse bignum1) 0))
                                                             |  (signal1 /= signal2) && (checkBiggestNum bigNum1 bigNum2) = (signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0))
                                                             |  otherwise = (signal2, reverse(sumBefore (reverse bignum2) (reverse bignum1) 0))




subBN :: BigNumber -> BigNumber -> BigNumber
subBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) |  signal1 && signal2 = (signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0))
                                                             |  (signal1 /= signal2) && length bignum1 > length bignum2 = (signal1, reverse(subBefore (reverse bignum1) (reverse bignum2) 0))
                                                             |  (signal1 /= signal2) && length bignum2 > length bignum1 = (signal2, reverse(subBefore (reverse bignum2) (reverse bignum1) 0))
                                                             |  otherwise = (signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0))




-- -- Aplic
-- sumBefore :: BigNumber -> BigNumber -> Int -> BigNumber   
-- sumBefore [] bigNum@(signal, bignum) carry 
--         | carry == 0 = bignum
--         | otherwise = sumBefore [carry] list 0

-- sumBefore bigNum@(signal, bignum) [] carry
--         | carry == 0 = list
--         | otherwise = sumBefore list [carry] 0

-- sumBefore (x:xs) (y:ys) c = lastDigit : sumBefore xs ys rest
--         where sumNum = x + y + c
--               lastDigit = mod sumNum 10  -- Encontra o último dígito de sumNum, p.e: mod 15 10 = 5 
--               rest = div sumNum 10       -- Encontra o resto da divisão inteira de sumNumb por 10, p.e: div 15 10 = 1


-- somaBN :: BigNumber -> BigNumber -> BigNumber
-- somaBN bigNum1 bigNum2 | (head bigNum1) < 0 && (head bigNum2) > 0 = reverse(sumBefore (reverse (head bigNum1 : (map negate (tail bigNum1)))) (reverse bigNum2) 0 )
--                        | (head bigNum1) > 0 && (head bigNum2) < 0 = reverse(sumBefore (reverse bigNum1) (reverse (head bigNum2 : ((map negate (tail bigNum2))))) 0 )
--                        | otherwise = reverse(sumBefore (reverse bigNum1) (reverse bigNum2) 0 )


-- subBN :: BigNumber -> BigNumber -> BigNumber
-- subBN  bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) | signal1 && signal2 = (signal1, reverse(sumBefore (reverse bignum1) (reverse bignum2) 0))

mulAux :: BigNumber -> BigNumber -> BigNumber

mulAux nb1@(_, []) nb2@(_,_) = (False, [0])                 

mulAux nb1@(signal1, x:xs) nb2@(signal2, ys) = somaBN (False, (map (x*) ys)) (mulAux (False,xs) (False, (ys++ [0])))

mulBN :: BigNumber -> BigNumber -> BigNumber

mulBN bigNum1@(signal1, bignum1) bigNum2@(signal2, bignum2) = bigN3
        where
                ret@(signal3, bignum3) = mulAux (signal1, reverse bignum1) bigNum2
                bigN3 = (signal1 || signal2, bignum3)

