--Questão 8:
remover_repetidos :: Eq a => [a] -> [a]
remover_repetidos lista = aux lista []
-- chama uma função auxiliar "aux", passando a lista original 
-- e uma lista vazia "[]", servindo de memoria
  where
    aux [] _ = [] -- A lista original estiver vazia "[]", função retorna uma lista vazia "[]"
    aux (x:xs) vistos 
        | x `elem` vistos = aux xs vistos -- "x" estiver presente na lista vistos, a função "aux" é chamada recursivamente 
        | otherwise = x : aux xs (x : vistos) 
        -- "x" não estiver presente na lista vistos, é adicionado à lista de resultados


--Questão 17:
uniao :: Eq a => [a] -> [a] -> [a]
uniao xs ys = xs ++ filtrar ys xs -- A função "uniao" recebe duas listas "xs" e "ys", e retorna a união dessas listas
  where
    filtrar [] _ = [] -- segunda lista (que estamos percorrendo) ficar vazia, retornamos vazio
    filtrar (z:zs) base -- função "filtrar" recebe a segunda lista (z:zs) e a primeira lista (base) como argumentos
        | z `elem` base = filtrar zs base 
       -- "z" estiver presente na lista base, a função "filtrar" é chamada recursivamente
        | otherwise = z : filtrar zs base
       -- "z" não estiver presente na lista base, então "z" é adicionado à lista de resultados 


--Questão 26:
todas_maiusculas :: String -> String
todas_maiusculas [] = [] -- Se a string acabar, retorna uma string vazia
todas_maiusculas (x:xs) 
    | num >= 97 && num <= 122 = toEnum (num - 32) : todas_maiusculas xs 
  -- Se "x" for uma letra minúscula, converte para maiúscula subtraindo 32 do valor numérico e adiciona à lista de resultados
    | otherwise = x : todas_maiusculas xs 
  -- Se "x" não for uma letra minúscula, mantém o caractere original e continua a função recursivamente com o restante da string
  where 
    num = fromEnum x 
  -- converte o caractere "x" para seu valor numérico usando "fromEnum"


--Questão 35:
soma_digitos :: Integer -> Integer
soma_digitos n = aux n 0 -- A função "soma_digitos" recebe um número inteiro "n" e retorna a soma de seus dígitos.
  where
    aux 0 acc = acc 
  -- Se o número for 0, retorna o acumulador "acc", que contém a soma dos dígitos
    aux x acc = aux (x `div` 10) (acc + (x `mod` 10)) 
  -- Recebe o número atual "x" e o total parcial "acc"
  -- A função "aux" é chamada recursivamente, onde ("x `div` 10") remove o último dígito de "x" 
  -- (acc + (x `mod` 10))" adiciona o último dígito de "x" ao acumulador "acc"