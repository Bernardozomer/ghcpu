import Data.Word

main = do
    contents <- readFile "test"
    let mem = Mem (map readVal . words $ contents)
    print mem

-- Execute instruction cycles until the processor is halted.
exeCycles :: (CPUState, Mem) -> (CPUState, Mem)
exeCycles (state, mem) = case state of
    Halted -> (state, mem)
    otherwise -> exeCycles (exeStage (state, mem))

-- Execute a single stage in the instruction cycle.
exeStage :: (CPUState, Mem) -> (CPUState, Mem)
exeStage (CPUState stage regs, mem) = case stage of
    Fetch -> (CPUState Decode (fetchInstr regs mem), mem)
    Decode -> (CPUState (Execute (decodeInstr (regIR regs))) regs, mem)
    Execute instr -> exeInstr (CPUState stage regs, mem)

-- Fetch the next instruction at memory locations regIC and regIC + 1
-- and increment regIC in 2.
fetchInstr :: Regs -> Mem -> Regs
fetchInstr regs mem = regs {
        regIC = (regIC regs) + (Ptr 2),
        regIR = (
                readMem (regIC regs) mem,
                Ptr (valToWord8 (readMem ((regIC regs) + (Ptr 1)) mem))
            )
    }

-- Decode a 16-bit instruction code.
decodeInstr :: (Val, Ptr) -> Instr
decodeInstr (Val val, y)
    | val == 2  = Lod y
    | val == 4  = Sto y
    | val == 6  = Jmp y
    | val == 8  = Jmz y
    | val == 10 = Cpe y
    | val == 14 = Add y
    | val == 16 = Sub y
    | val == 18 = Nop
    | val == 20 = Hlt

exeInstr :: (CPUState, Mem) -> (CPUState, Mem)
exeInstr (CPUState (Execute instr) regs, mem) = case instr of
    Lod ptr -> (fetch (writeToRegACC (readMem ptr mem) regs), mem)
    Sto ptr -> (fetch regs, writeToMem ptr (regACC regs) mem)
    Jmp ptr -> (fetch regs { regIC = ptr }, mem)
    Jmz ptr -> (fetch regs { regIC = newRegIC }, mem)
        where newRegIC = if regEQZ regs then ptr else regIC regs
    Cpe ptr -> (fetch (writeToRegACC newRegACC regs), mem)
        where newRegACC = if readMem ptr mem == regACC regs then Val 0 else Val 1
    Add ptr -> (fetch (writeToRegACC sumRes regs), mem)
        where sumRes = regACC regs + readMem ptr mem
    Sub ptr -> (fetch (writeToRegACC subRes regs), mem)
        where subRes = regACC regs - readMem ptr mem
    Nop -> (fetch regs, mem)
    Hlt -> (Halted, mem)
    where fetch = CPUState Fetch

writeToRegACC :: Val -> Regs -> Regs
writeToRegACC val regs = regs { regACC = val, regEQZ = (val == (Val 0)) }

readMem :: Ptr -> Mem -> Val
readMem (Ptr ptr) (Mem mem) = mem !! (fromIntegral ptr)

writeToMem :: Ptr -> Val -> Mem -> Mem
writeToMem (Ptr ptr) val (Mem mem) = Mem (setAt (fromIntegral ptr) val mem)

ptrToWord8 :: Ptr -> Word8
ptrToWord8 (Ptr a) = a

valToWord8 :: Val -> Word8
valToWord8 (Val a) = a

-- Casts a String to a Val
readVal :: String -> Val
readVal word = Val (read word)

-- Source: https://stackoverflow.com/a/5852820/7834359
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:setAt (n-1) newVal xs

data Instr = Lod Ptr | Sto Ptr | Jmp Ptr | Jmz Ptr
    | Cpe Ptr | Add Ptr | Sub Ptr | Nop | Hlt deriving (Show)

data CPUState = CPUState Stage Regs | Halted
data Stage = Fetch | Decode | Execute Instr

data Regs = Regs {
    regACC :: Val,
    regEQZ :: Bool,
    regIC  :: Ptr,
    regIR  :: (Val, Ptr)
} deriving (Show)

data Mem = Mem [Val] deriving (Show)
newtype Ptr = Ptr Word8 deriving (Eq, Show)

instance Num Ptr where
    Ptr a + Ptr b = Ptr (a + b)
    Ptr a - Ptr b = Ptr (a - b)

newtype Val = Val Word8 deriving (Eq, Show)

instance Num Val where
    Val a + Val b = Val (a + b)
    Val a - Val b = Val (a - b)
