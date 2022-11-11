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
exeStage (CPUState stage regs, RAM ram) = case stage of
	Fetch -> (
			CPUState Decode (fetchNextInstr regs (RAM ram)) {
					regIC = (readRegIC regs) + (Ptr 2)
				},
			(RAM ram)
		)
	Decode -> (CPUState (Execute (decodeInstr (readRegIR regs))) regs, RAM ram)
	Execute instr -> case instr of
		Lod ptr -> (
				CPUState Fetch (writeToRegACC (readMem ptr (RAM ram)) regs),
				RAM ram
			)
		Sto ptr -> (
				CPUState Fetch regs,
				writeToMem ptr (readRegACC regs) (RAM ram)
			)
		Jmp ptr -> (CPUState Fetch regs { regIC = ptr }, RAM ram)
		Jmz ptr -> if readRegEQZ regs
			then (CPUState Fetch regs { regIC = ptr }, RAM ram)
			else (CPUState Fetch regs, RAM ram)
		Cpe ptr -> if readMem ptr (RAM ram) == readRegACC regs
			then (CPUState Fetch (writeToRegACC 0 regs), RAM ram)
			else (CPUState Fetch (writeToRegACC 1 regs), RAM ram)
		Add ptr -> (
				CPUState Fetch (writeToRegACC (
						readRegACC regs + readMem ptr (RAM ram)) regs
					),
				RAM ram
			)
		Sub ptr -> (
				CPUState Fetch (writeToRegACC (
						readRegACC regs - readMem ptr (RAM ram)) regs
					),
				RAM ram
			)
		Nop -> (CPUState stage regs, RAM ram)
		Hlt -> (Halted, RAM ram)

fetchNextInstr :: Regs -> RAM -> Regs
fetchNextInstr regs ram = regs {
		regIR = (
				readMem (readRegIC regs) ram,
				Ptr (valToWord8 (readMem ((readRegIC regs) + (Ptr 1)) ram))
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

readRegACC :: Regs -> Val
readRegACC (Regs regACC _ _ _) = regACC

readRegEQZ :: Regs -> Bool
readRegEQZ (Regs _ regEQZ _ _) = regEQZ

readRegIC :: Regs -> Ptr
readRegIC (Regs _ _ regIC _) = regIC

readRegIR :: Regs -> (Val, Ptr)
readRegIR (Regs _ _ _ regIR) = regIR

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
