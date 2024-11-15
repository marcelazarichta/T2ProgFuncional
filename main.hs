import System.IO (readFile)
import Data.List (lines)
import Data.Char (isSpace)

data Computador = Computador {
    mem :: [(Int, Int)], -- memória 
    acc :: Int, -- registrador acumulador
    pc :: Int, -- contador de instruções
    ir :: Int, -- armazena a instrucao atual (instruction register)
    eqz :: Int -- flag zero
} deriving (Show) -- permite que os valores sejam exibidos como String


inicializaComputador :: [(Int, Int)] -> Computador
inicializaComputador memoria = Computador {
    mem = memoria,  
    acc = 0,               
    pc = 0,               
    ir = 0,               
    eqz = 1 -- 0 = não é zero
} 

--testePrograma :: [(Int, Int)]
--testePrograma = [
--    (0, 2),    -- LOD 240
--    (1, 240), 
--    (2, 14),   -- ADD 241
--    (3, 241),  
--    (4,4),     -- STO 251
--    (5,251),
--    (6,20),    -- HTL 
--    (7,18),
--    (240, 0),  -- Endereço com valor 1
--    (241, 1),  -- Endereço com valor 2
--    (251, 0)   -- Endereço que será atualizado
--    ]

--testePrograma2 :: [(Int, Int)]
--testePrograma2 =  [
--    (0,2), (1,240), -- LOD 240
--    (2,14), (3,241), -- ADD 241
--    (4,16), (5,242), -- SUB 242
--    (6,4), (7,251), -- STO 251
--    (8,20), (9,18), -- HTL NOP
--    (240,12),
--    (241,3),
--    (242,2),
--    (251,0)
--    ]
    
    

executaCiclo :: Computador -> Computador
executaCiclo computador
    | pc computador == -1 = computador  
    | otherwise = executaCiclo (executaInstrucao computador)


executaCicloComImpressao :: Computador -> IO Computador
executaCicloComImpressao computador
    | pc computador == -1 = return computador
    | otherwise = do
        print computador 
        let computadorNovo = executaInstrucao computador
        executaCicloComImpressao computadorNovo


executaInstrucao :: Computador -> Computador
executaInstrucao computador =
    let irNovo = readMem (mem computador) (pc computador);    
        argumento = readMem (mem computador) (pc computador + 1)
        computadorNovo = computador {ir = irNovo}
    in case irNovo of
        2  -> execLOD argumento computadorNovo    
        4  -> execSTO argumento computadorNovo  
        14 -> execADD argumento computadorNovo     
        16 -> execSUB argumento computadorNovo   
        18 -> execNOP computadorNovo      
        20 -> execHLT computadorNovo         
        _  -> computadorNovo                 -- Caso não seja reconhecido, não faz nada 


-- Intrução LOD
-- Carrega o conteúdo do endereço de memória no registrador acumulador
execLOD :: Int -> Computador -> Computador
execLOD endereco computador =
    let novoAcc = truncamento (readMem (mem computador) endereco)
    in computador {pc = pc computador + 2, acc = novoAcc, eqz = if novoAcc == 0 then 1 else 0}


-- Instrucao STO
-- Armazena o conteúdo do registrador acumulador (ACC) no endereço de memória 
execSTO :: Int -> Computador -> Computador
execSTO endereco computador = 
    let novaMem = writeMem (mem computador) endereco (acc computador)
    in computador {pc = pc computador + 2, mem = novaMem}
    

-- Instrucao JMP
-- Desvio incondicional: carrega no contador de instruções o valor forçando com que
-- a próxima instrução a ser executada seja a que se encontra no endereço de memória.
execJMP :: Int -> Computador -> Computador
execJMP endereco computador = computador {pc = endereco}

-- Instrucao JMZ
-- Desvio condicional: funcionamento análogo ao da instrução JMP com a diferença que
-- a carga do contador de instruções só ocorre se o valor do acumulador for
-- igual a zero (de acordo com a flag EQZ).
execJMZ :: Int -> Computador -> Computador
execJMZ endereco computador 
    | eqz computador == 1 = computador {pc = endereco}
    | eqz computador /= 1 = computador {pc = pc computador + 2}


-- Instrucao CPE
-- Se o conteúdo do endereço for igual ao acumulador, coloca 0 no acumulador,
-- caso contrário coloca 1.
execCPE :: Int -> Computador -> Computador
execCPE endereco computador  
    | readMem (mem computador) endereco == acc computador = computador {acc = 0, eqz = 1, pc = pc computador + 2}
    | readMem (mem computador) endereco /= acc computador = computador {acc = 1, eqz = 0, pc = pc computador + 2}


-- Instrucao ADD
-- Adiciona o conteúdo do endereço de memória ao conteúdo armazenado no 
-- acumulador (ACC) e armazena a resposta no próprio acumulador.
execADD :: Int -> Computador -> Computador
execADD endereco computador = 
    let novoAcc = truncamento ((acc computador) + readMem (mem computador) endereco)
    in computador {pc = pc computador + 2, acc = novoAcc, eqz = if novoAcc == 0 then 1 else 0}


-- Instrucao SUB
-- Subtrai o conteúdo do endereço de memória do conteúdo do 
-- acumulador (ACC) e armazena a resposta no próprio acumulador.
execSUB :: Int -> Computador -> Computador
execSUB endereco computador = 
    let novoAcc = truncamento ((acc computador) - readMem (mem computador) endereco)
    in computador {pc = pc computador + 2, acc = novoAcc, eqz = if novoAcc == 0 then 1 else 0}


-- Instrução NOP
execNOP :: Computador -> Computador
execNOP computador = computador {pc = pc computador + 2}

-- Instrucao HTL
-- Encerra o ciclo de execução do processador (HaLT)
execHLT :: Computador -> Computador
execHLT computador = computador {pc = -1} 

-- 
truncamento :: Int -> Int
truncamento numero
    | numero < -128  = -128     
    | numero > 127   = 127      
    | numero > -128 && numero < 177 = numero        



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

carregarMemoriaDeArquivo :: FilePath -> IO [(Int, Int)]
carregarMemoriaDeArquivo caminho = do
    conteudo <- readFile caminho
    return (map linhaParaTupla (lines conteudo))

linhaParaTupla :: String -> (Int, Int)
linhaParaTupla linha = 
    let [enderecoStr, valorStr] = words linha
    in (read enderecoStr :: Int, read valorStr :: Int)


main :: IO ()
main = do
    -- Carrega a memória do arquivo "memoria.txt"
    memoria <- carregarMemoriaDeArquivo "memoria.txt"  -- Caminho do arquivo
    putStrLn "Memória carregada do arquivo memoria.txt:"
    print memoria  -- Imprime a memória para verificação
    let computador = inicializaComputador memoria
    computadorFinal <- executaCicloComImpressao computador
    putStrLn ""
    putStrLn "Estado final do computador:"
    print computadorFinal

