{-
=============== Grupo 02 ===============

Bianca Santos de Assis - 202510351
Gabriel Pirassoli Mendonça Pereira Vale - 202510367
José Geraldo Caria da Silva - 202410188

========================================
-}

-- ===== Exercício 5 ===== --

-- O tipo [a] -> [a] -> [a] garante o polimorfismo (recebe duas listas de mesmo tipo e retorna uma nova com o mesmo tipo).
concatena :: [a] -> [a] -> [a]
-- Caso base: primeira lista vazia
concatena [] lista2 = lista2
-- Caso recursivo: desmembra a primeira lista e usa recursão
concatena (x:xs) lista2 = x : concatena xs lista2

-- ===== Exercício 14 ===== --

-- recebe lista generica de faz parte da classe Números
somatorio :: Num a => [a] -> a
-- Caso base: lista vazia
somatorio [] = 0
--Caso recursivo: desmembra a lista e usa recursão
somatorio (x:xs) = x + somatorio xs

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
