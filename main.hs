data Computador = Computador {
    mem :: [(Int, Int)], -- memória 
    acc :: Int, -- registrador acumulador
    pc :: Int, -- contador de instruções
    ir :: Int, -- armazena a instrucao atual
    eqz :: Int -- flag zero
} deriving (Show) -- permite que os valores sejam exibidos como String

inicializaComputador :: [(Int, Int)] -> Computador
inicializaComputador testProgram Computador {
    mem = memoriaInicial,  
    acc = 0,               
    pc = 0,               
    ir = 0,               
    eqz = 0                -- 0 = não é zero
} 


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
-- Carrega o conteúdo do endereço de memória no registrador acumulador
-- execLOD(end,mem,acc,eqz)=(mem,acc,eqz)
execLOD :: Int -> ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execLOD end (mem, acc, eqz) = (mem, readMem mem end, eqz)

-- Instrucao STO
-- Armazena o conteúdo do registrador acumulador (ACC) no endereço de memória 
--execSTO(end,mem,acc,eqz) = (mem, acc, eqz)
execSTO :: Int -> ([(Int,Int)], Int, Int) -> ([(Int, Int)], Int, Int)
execSTO end (mem, acc, eqz) = (writeMem mem end acc, acc, eqz)

-- Instrucao JMP
-- Desvio incondicional: carrega no contador de instruções o valor forçando com que
-- a próxima instrução a ser executada seja a que se encontra no endereço de memória.


-- Instrucao JMZ

-- Instrucao CPE
-- Se o conteúdo do endereço for igual ao acumulador, coloca 0 no acumulador,
-- caso contrário coloca 1.
-- execCPE (end, mem, acc, eqz) = (mem, acc, eqz) 
execCPE :: Int -> ([(Int, Int)], Int, Int) -> ([(Int, Int)], Int, Int)
execCPE end (mem, acc, eqz) 
    | readMem mem end == acc = (mem, 0, eqz)
    | readMem mem end /= acc = (mem, 1, eqz)

-- Instrucao ADD
-- Adiciona o conteúdo do endereço de memória ao conteúdo armazenado no 
-- acumulador (ACC) e armazena a resposta no próprio acumulador.
-- execADD (end, mem, acc, eqz) = (acc, mem, eqz)
execADD :: Int -> ([(Int, Int)], Int, Int) -> ([(Int, Int)], Int, Int)
execADD end (mem, acc, eqz) = (mem, acc + readMem mem end, eqz)


-- Instrucao SUB
-- Subtrai o conteúdo do endereço de memória do conteúdo do 
-- acumulador (ACC) e armazena a resposta no próprio acumulador.
-- execSUB (end, mem, acc, eqz) = (acc, mem, eqz)
execSUB :: Int -> ([(Int, Int)], Int, Int) -> ([(Int, Int)], Int, Int)
execSUB end (mem, acc, eqz) = (mem, acc - readMem mem end, eqz)

-- Instrução NOP
-- execNOP(memoria,acc,eqz)=(memoria,acc,eqz)
execNOP :: ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execNOP (mem, acc, eqz) = (mem, acc, eqz)

-- Instrucao HTL

-- Retorna o que está armazenado no endereço de memória recebido
--readMem(memoria,end)=conteudo
readMem :: [(Int,Int)] -> Int -> Int
readMem (m:ms) end
    | end == fst m = snd m
    | end /= fst m = readMem ms end

-- Armazenar o conteúdo em um endereço de memória
-- writeMem(mem,end,conteudo) = memoria
writeMem :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
writeMem (m:ms) end valor
    | end == fst m = (end, valor) : ms
    | otherwise = m : writeMem ms end valor