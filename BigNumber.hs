import Data.Char



-- Definição do tipo BigNumber

type BigNumber = [Int]    

-- Por exemplo -12455 => [-1,2,4,5,5]


-- Aplica a função read após aplicar a função (:"") a cada um dos elementos da string str (lista de chars) 
-- criando listas individuais de cada um dos números e depois aplica map para criar uma lista de todos os dígitos de str.

scanner :: String -> BigNumber
scanner str | (head str) == '-' = reverse(negate( read (head (tail str) : "") :: Int) : (map (read . (:"")) (tail $ tail str) :: BigNumber))
            | otherwise = reverse(map (read . (:"")) str :: BigNumber)


-- Aplica a função show que dá preapend a todos os elementos de bigNum através da função map

output :: BigNumber -> String 
output bigNum = concat(map show bigNum)

                                    --carry    


soma :: BigNumber -> BigNumber -> Int -> BigNumber   
soma [] list carry 
        | carry == 0 = list
        | otherwise = soma [carry] list 0
soma list [] carry
        | carry == 0 = list
        | otherwise = soma list [carry] 0
soma (x:xs) (y:ys) c = dig : soma xs ys rst
        where soma = x + y + c 
              (rst, dig) = soma `divMod` 10


somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN bigNum1 bigNum2 = reverse(soma bigNum1 bigNum2 0)