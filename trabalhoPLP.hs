{-
=============== Grupo 02 ===============

Bianca Santos de Assis - 202510351
Gabriel Pirassoli Mendonça Pereira Vale - 202510367
José Geraldo Caria da Silva - 202410188

========================================
-}

-- ===== Exercício 2 ===== --

insere_no_fim :: t -> [t] -> [t]
insere_no_fim item [] = [item]
    -- se a lista estiver vazia, insere o elemento como o único da lista
insere_no_fim item (c:r) = c : insere_no_fim item r
    -- mantem a cabeça e continua procurando o fim da lista recursivamente para inserir o item

-- ===== Exercício 5 ===== --

-- O tipo [a] -> [a] -> [a] garante o polimorfismo (recebe duas listas de mesmo tipo e retorna uma nova com o mesmo tipo).
concatena :: [a] -> [a] -> [a]
-- Caso base: primeira lista vazia
concatena [] lista2 = lista2
-- Caso recursivo: desmembra a primeira lista e usa recursão
concatena (x:xs) lista2 = x : concatena xs lista2

-- ===== Exercício 8 ===== --

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

-- ===== Exercício 11 ===== --

reverso :: [t] -> [t]
reverso [] = []
    -- o reverso de uma lista vazia é ela mesma
reverso (c:r) = insere_no_fim c (reverso r)
    -- inverte o resto e coloca a cabeça no fim

-- ===== Exercício 14 ===== --

-- recebe lista generica de números
somatorio :: Num a => [a] -> a
-- Caso base: lista vazia
somatorio [] = 0
--Caso recursivo: desmembra a lista e usa recursão
somatorio (x:xs) = x + somatorio xs

-- ===== Exercício 17 ===== --

uniao :: Eq a => [a] -> [a] -> [a]
uniao xs ys = xs ++ filtrar ys xs -- A função "uniao" recebe duas listas "xs" e "ys", e retorna a união dessas listas
  where
    filtrar [] _ = [] -- segunda lista (que estamos percorrendo) ficar vazia, retornamos vazio
    filtrar (z:zs) base -- função "filtrar" recebe a segunda lista (z:zs) e a primeira lista (base) como argumentos
        | z `elem` base = filtrar zs base 
       -- "z" estiver presente na lista base, a função "filtrar" é chamada recursivamente
        | otherwise = z : filtrar zs base
       -- "z" não estiver presente na lista base, então "z" é adicionado à lista de resultados 
    
-- ===== Exercício 20 ===== --

insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado item [] = [item] 
    -- se a lista estiver vazia, insere o elemento como o único da lista
insere_ordenado item (c:r)
    | item <= c = item : c : r 
        -- se for menor ou igual, insere antes da cabeça
    | otherwise = c : insere_ordenado item r 
        -- se for maior, mantém a cabeça e continua procurando no resto da lista

-- ===== Exercício 23 ===== --

picos :: Ord a => [a] -> [a]
picos lista
    -- Se a lista tiver menos de 3 elementos, não forma um círculo válido para picos
    | length lista < 2 = [] 
    | otherwise        = encontrarOsPicos
 listaPreparada
  where
    primeiro       = head lista
    ultimo         = last lista
    -- Montamos: [último] colado na lista original colada no [primeiro]
    listaPreparada = [ultimo] ++ lista ++ [primeiro]

-- recebe lista generica 
encontrarOsPicos :: Ord a => [a] -> [a]
-- Caso base: lista vazia
encontrarOsPicos [] = []
-- Caso base: lista com um elemento
encontrarOsPicos [x] = [x]
-- Caso recursivo: desmembra a lista e usa recursão
encontrarOsPicos (x:y:z:xs)
    | y > x && y > z = y : encontrarOsPicos
 (y:z:xs)
    |otherwise = encontrarOsPicos
 (y:z:xs)
--Para listas com menos de 3 itens
encontrarOsPicos _ = []

-- ===== Exercício 26 ===== --

todas_maiusculas :: String -> String
todas_maiusculas [] = [] -- Se a string acabar, retorna uma string vazia
todas_maiusculas (x:xs) 
    | num >= 97 && num <= 122 = toEnum (num - 32) : todas_maiusculas xs 
  -- Se "x" for uma letra minúscula, converte para maiúscula subtraindo 32 do valor numérico e adiciona à lista de resultados
    | otherwise = x : todas_maiusculas xs 
  -- Se "x" não for uma letra minúscula, mantém o caractere original e continua restante da string
  where 
    num = fromEnum x 
  -- converte o caractere "x" para seu valor numérico usando "fromEnum"

-- ===== Exercício 29 ===== --

{-- uma forma de calcular a variancia é: 
Variancia = média dos quadrados − (média)² --}

-- Funções auxiliares para calcular a variância --
soma [] = 0
soma (c:r) = c + soma r

tamanho [] = 0
tamanho (c:r) = 1 + tamanho r

quadrados [] = []
quadrados (c:r) = (c * c) : quadrados r

variancia :: Fractional t => [t] -> t
variancia [] = 0
variancia lista = 
    if tamanho lista == 0  then 0 
        -- se o tamanho da lista for zero, retorna que a variancia é zero
    else (soma (quadrados lista) / tamanho lista) - 
    ((soma lista / tamanho lista) * (soma lista / tamanho lista))
        -- faz a média dos números ao quadrado menos o quadrado da média normal

-- ===== Exercício 32 ===== --
-- Recebe lista generica de numeros e que podem ser comparados e devolve um lista de listas
separa :: (Eq a, Num a) => [a] -> [[a]]
-- Caso base: retorna uma lista contendo uma sublista vazia
separa [] = [[]]
-- Caso recursivo: desmembra a lista e usa recursão
separa (x:xs)
    | x == 0    = [] : separa xs
    | otherwise = adiciona x (separa xs)

-- Função auxiliar para colocar um elemento dentro da primeira sublista
adiciona :: a -> [[a]] -> [[a]]
adiciona x (ys:yss) = (x:ys) : yss
adiciona x [] = [[x]]

-- ===== Exercício 35 ===== --

soma_digitos :: Integer -> Integer
soma_digitos n = aux n 0 -- A função "soma_digitos" recebe um número inteiro "n" e retorna a soma de seus dígitos.
  where
    aux 0 acc = acc 
  -- Se o número for 0, retorna o acumulador "acc", que contém a soma dos dígitos
    aux x acc = aux (x `div` 10) (acc + (x `mod` 10)) 
  -- Recebe o número atual "x" e o total parcial "acc"
  -- A função "aux" é chamada recursivamente, onde ("x `div` 10") remove o último dígito de "x" 
  -- (acc + (x `mod` 10))" adiciona o último dígito de "x" ao acumulador "acc"

-- ===== Exercício 38 ===== --

{- sabendo que qualquer quadrado perfeito pode ser representado pela soma
dos n primeiros números ímpares, podemos criar uma função que pega o valor
e vai subtraindo pelos n primeiros números ímpares até que o valor seja
zero (se for true) ou fique negativo (se for false) -}

quadrado_perfeito :: Integral t => t -> Bool
quadrado_perfeito n = subtrai_impares n 1

subtrai_impares n atual
    | n == 0    = True   -- se zerou, é quadrado perfeito
    | n < 0     = False  -- Se ficou negativo, não é quadrado perfeito
    | otherwise = subtrai_impares (n - atual) (atual + 2) 
        -- subtrai o número ímpar atual e passa para o próximo número ímpar
