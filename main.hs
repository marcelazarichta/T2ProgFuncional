data Computador = Computador {
    mem :: [(Int, Int)],
    acc :: Int,
    pc :: Int,
    ir :: Int,
    eqz :: Int
} deriving (Show)



prog1 :: [(Int,Int)]

-- Intrução LOD
-- execLOD(endereco,mem,acc,eqz)=(mem,acc,eqz)
execLOD :: Int -> ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execLOD end (mem, acc, eqz) = (mem, readMem mem end, eqz)

-- Instrucao STO

-- Instrucao JMP

-- Instrucao JMZ

--Instrucao CPE

-- Instrucao ADD

-- Instrucao SUB


-- Instrução NOP
-- execNOP(memoria,acc,eqz)=(memoria,acc,eqz)
execNOP :: ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execNOP (mem, acc, eqz) = (mem, acc, eqz)

-- Instrucao HTL

-- Retorna o que está armazenado no endereço de memória recebido
readMem :: [(Int,Int)] -> Int -> Int
readMem (m:ms) e
    | e == fst m = snd m
    | e /= fst m = readMem ms e

-- writeMem(memoria,endereço,conteudo) = memoria
writeMem :: ([(Int,Int), Int, Int]) -> [Int,Int]