-- Author: Arthur Moreira Correa NUSP: 13749952
import Data.List (intercalate)
import Control.Monad.RWS.Class (MonadState(put))
import Data.Traversable (for, forM)

main = do
    --Leitura e separação dos números inteiros
    userInput <- getLine
    let moves = map read (words userInput) :: [Integer]
    let frames = frameBuilderInit moves
    putStrLn
        $ formatScore frames
        ++ " | "
        ++ show (scoreCalculator frames)

-- Função que inicia a criaçao de uma lista de frames a partir de uma lista de números
frameBuilderInit :: (Num a, Eq a) => [a] -> [[a]]
frameBuilderInit list = frameBuilder list 1

-- Função que cria uma lista de frames a partir de uma lista de números
frameBuilder :: (Num a, Eq a) => [a] -> a -> [[a]]
-- só temos 10 frames no boliche
frameBuilder _ 11 = []
frameBuilder (10:xs) fNum
    | fNum == 10 = [10 : take 2 xs]
    | otherwise = [10] : frameBuilder xs (fNum + 1)
frameBuilder (n1:n2:ns) fNum
    | fNum == 10 = [[n1, n2] ++ take 1 ns]
    | n1 + n2 == 10 = [n1, n2] : frameBuilder ns (fNum + 1)
    | otherwise = [n1, n2] : frameBuilder ns (fNum + 1)
frameBuilder x _ = [x]

-- Função que calcula a pontuação total de uma partida de boliche
scoreCalculator :: (Num a, Eq a) => [[a]] -> a
scoreCalculator frames = sum (zipWith frameScore frames (drop 1 (subListas frames)))

-- Função que auxilia a função scoreCalculator, calculando a pontuação de um frame específico
frameScore :: (Num a, Eq a) => [a] -> [[a]] -> a
frameScore [10] (f1:f2:_) = 10 + (sum $ take 2 (f1 ++ f2))
frameScore [10] (f1:_) = 10 + (sum $ take 2 f1)
frameScore [n1, n2] (f1:_)
    | n1 + n2 == 10 = 10 + head f1
    | otherwise = n1 + n2
frameScore frame _ = sum frame

subListas :: [a] -> [[a]]
subListas [] = [[]]
subListas (x:xs) = (x:xs) : subListas xs

-- Função que formata os frames para a saída
formatScore :: [[Integer]] -> String
formatScore = intercalate " | " . map formatFrame

-- Função auxiliar que formata um frame específico que será utilizado na função formatScore em uma concatenação com intercalate que é equivalente a um concat interperse xs xss
formatFrame :: [Integer] -> String
formatFrame [10] = "X _"
formatFrame[10,10] = "X X"
formatFrame [n1, n2]
    | n1 + n2 == 10 = show n1 ++ " /"
    | otherwise = show n1 ++ " " ++ show n2
formatFrame [n1, n2, n3]
    | n1 == 10 = "X " ++ formatFrame [n2, n3]
    | n1 + n2 == 10 = show n1 ++ " / " ++ formatNumber n3
    | otherwise = show n1 ++ " " ++ show n2 ++ " " ++ show n3

-- Função auxiliar que formata um número para a saída
formatNumber :: Integer -> String
formatNumber 10 = "X"
formatNumber n = show n