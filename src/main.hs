import Data.Word

main = do
	contents <- readFile "test"
	let ram = map (read::String->Word8) . words $ contents
	print $ show ram

-- Execute instruction cycles until the processor is halted.
exeCycles :: (CPUState, RAM) -> (CPUState, RAM)
exeCycles (state, ram) = case state of
    Halted -> (state, ram)
    otherwise -> exeCycles (exeStage (state, ram))

-- Execute a stage in the instruction cycle.
exeStage :: (CPUState, RAM) -> (CPUState, RAM)
exeStage (CPUState stage regs, ram) = case stage of
	Fetch -> (
			CPUState Decode (fetchNextInstr regs ram) {
					regIC = (regIC regs) + (Ptr 2)
				},
			ram
		)
	Decode -> (CPUState (Execute (decodeInstr (regIR regs))) regs, ram)
	Execute instr -> exeInstr (CPUState stage regs, ram)

fetchNextInstr :: Regs -> RAM -> Regs
fetchNextInstr regs ram = regs {
		regIR = (
				readMem (regIC regs) ram,
				Ptr (valToWord8 (readMem ((regIC regs) + (Ptr 1)) ram))
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

exeInstr :: (CPUState, RAM) -> (CPUState, RAM)
exeInstr (CPUState (Execute instr) regs, ram) = case instr of
		Lod ptr -> (CPUState Fetch (writeToRegACC (readMem ptr ram) regs), ram)
		Sto ptr -> (CPUState Fetch regs, writeToMem ptr (regACC regs) ram)
		Jmp ptr -> (CPUState Fetch regs { regIC = ptr }, ram)
		Jmz ptr -> if regEQZ regs
			then (CPUState Fetch regs { regIC = ptr }, ram)
			else (CPUState Fetch regs, ram)
		Cpe ptr -> if readMem ptr ram == regACC regs
			then (CPUState Fetch (writeToRegACC 0 regs), ram)
			else (CPUState Fetch (writeToRegACC 1 regs), ram)
		Add ptr -> (
				CPUState Fetch (writeToRegACC (
						regACC regs + readMem ptr ram) regs
					),
				ram
			)
		Sub ptr -> (
				CPUState Fetch (writeToRegACC (
						regACC regs - readMem ptr ram) regs
					),
				ram
			)
		Nop -> (CPUState Fetch regs, ram)
		Hlt -> (Halted, ram)

writeToRegACC :: Val -> Regs -> Regs
writeToRegACC val regs = if val == 0
	then regs { regACC = val, regEQZ = True }
	else regs { regACC = val, regEQZ = False }

readMem :: Ptr -> RAM -> Val
readMem (Ptr ptr) (RAM ram) = ram !! (fromIntegral ptr)

writeToMem :: Ptr -> Val -> RAM -> RAM
writeToMem (Ptr ptr) val (RAM ram) = RAM (setAt (fromIntegral ptr) val ram)

ptrToWord8 :: Ptr -> Word8
ptrToWord8 (Ptr a) = a

valToWord8 :: Val -> Word8
valToWord8 (Val a) = a

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

data RAM = RAM [Val] deriving (Show)
newtype Ptr = Ptr Word8 deriving (Eq, Show)

instance Num Ptr where
	Ptr a + Ptr b = Ptr (a + b)
	Ptr a - Ptr b = Ptr (a - b)

newtype Val = Val Word8 deriving (Eq, Show)

instance Num Val where
	Val a + Val b = Val (a + b)
	Val a - Val b = Val (a - b)
