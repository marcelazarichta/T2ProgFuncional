data Computador = Computador {
    mem :: [(Int, Int)],
    acc :: Int,
    pc :: Int,
    ir :: Int,
    eqz :: Int
} deriving (Show)


testProgram :: [(Int, Int)]
testProgram = [
    (0, 2),    -- LOD 240
    (1, 240),  -- ADD 241
    (2, 14),   -- STO 251
    (3, 241),  -- HLT
    (240, 1),  -- Endereço com valor 1
    (241, 2),  -- Endereço com valor 2
    (251, 0)   -- Endereço que será atualizado
    ]

--prog1 :: [(Int,Int)]

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
readMem (m:ms) endereco
    | endereco == fst m = snd m
    | endereco /= fst m = readMem ms endereco

-- writeMem(memoria,endereço,conteudo) = memoria
writeMem :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
writeMem [] endereco valor = []
writeMem (m:ms) endereco valor
    | endereco == fst m = (endereco, valor) : ms
    | otherwise = m : writeMem ms endereco valor