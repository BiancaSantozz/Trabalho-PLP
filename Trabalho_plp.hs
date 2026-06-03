{-
=============== Grupo 02 ===============

Bianca Santos de Assis - 202510351
Gabriel Pirassoli Mendonça Pereira Vale - 202510367
José Geraldo Caria da Silva - 202410188

========================================
-}

module Trabalho_plp where

-- ===== Exercício 2 ===== --

insere_no_fim :: t -> [t] -> [t]
insere_no_fim item [] = [item]
    -- se a lista estiver vazia, insere o elemento como o único da lista
insere_no_fim item (c:r) = c : insere_no_fim item r
    -- mantem a cabeça e continua procurando o fim da lista recursivamente para inserir o item

-- ===== Exercício 11 ===== --

reverso :: [t] -> [t]
reverso [] = []
    -- o reverso de uma lista vazia é ela mesma
reverso (c:r) = insere_no_fim c (reverso r)
    -- inverte o resto e coloca a cabeça no fim
    
-- ===== Exercício 20 ===== --

insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado item [] = [item] 
    -- se a lista estiver vazia, insere o elemento como o único da lista
insere_ordenado item (c:r)
    | item <= c = item : c : r 
        -- se for menor ou igual, insere antes da cabeça
    | otherwise = c : insere_ordenado item r 
        -- se for maior, mantém a cabeça e continua procurando no resto da lista

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

-- =========================================== --

main :: IO ()
main = return ()
