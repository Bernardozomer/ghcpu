import Data.Word

-- Execute a stage in the instruction cycle.
exeStage :: (CPUState, RAM) ->  (CPUState, RAM)
exeStage (CPUState stage regs, RAM ram) = case stage of
	Decode -> (CPUState (Execute (decodeInstr (readRegIR regs))) regs, RAM ram)
	Execute instr -> case instr of
		Lod ptr -> (
				CPUState Fetch regs { regACC = readMem ptr (RAM ram) },
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
			then (CPUState Fetch regs { regACC = Val 0 }, RAM ram)
			else (CPUState Fetch regs { regACC = Val 1 }, RAM ram)
		Nop -> (CPUState stage regs, RAM ram)

-- Decode a 16-bit instruction code.
decodeInstr :: (Val, Ptr) -> Instr
decodeInstr  (Val 2, y) = Lod y
decodeInstr  (Val 4, y) = Sto y
decodeInstr  (Val 6, y) = Jmp y
decodeInstr  (Val 8, y) = Jmz y
decodeInstr (Val 10, y) = Cpe y
decodeInstr (Val 14, y) = Add y
decodeInstr (Val 16, y) = Sub y
decodeInstr (Val 18, y) = Nop
decodeInstr (Val 20, y) = Hlt

readRegPtr :: Reg -> Regs -> Ptr
readRegPtr reg (Regs _ _ regIC _ regMAR regMDR) = case reg of
	RegIC  -> regIC
	RegMAR -> regMAR
	RegMDR -> regMDR

readRegACC :: Regs -> Val
readRegACC (Regs regACC _ _ _ _ _) = regACC

readRegEQZ :: Regs -> Bool
readRegEQZ (Regs _ regEQZ _ _ _ _) = regEQZ

readRegIR :: Regs -> (Val, Ptr)
readRegIR (Regs _ _ _ regIR _ _) = regIR

readMem :: Ptr -> RAM -> Val
readMem (Ptr ptr) (RAM ram) = ram !! (fromIntegral ptr)

writeToMem :: Ptr -> Val -> RAM -> RAM
writeToMem (Ptr ptr) val (RAM ram) = RAM (setAt (fromIntegral ptr) val ram)

-- Source: https://stackoverflow.com/a/5852820/7834359
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:setAt (n-1) newVal xs

data Instr =
	  Lod Ptr
	| Sto Ptr
	| Jmp Ptr
	| Jmz Ptr
	| Cpe Ptr
	| Add Ptr
	| Sub Ptr
	| Nop
	| Hlt
	deriving (Show)

data CPUState = CPUState Stage Regs

data Stage =
	  Fetch
	| Decode
	| Execute Instr

data Regs = Regs {
	regACC :: Val,
	regEQZ :: Bool,
	regIC  :: Ptr,
	regIR  :: (Val, Ptr),
	regMAR :: Ptr,
	regMDR :: Ptr
} deriving (Show)

data Reg = RegACC | RegEQZ | RegIC | RegIR | RegMAR | RegMDR deriving (Show)
data RAM = RAM [Val] deriving (Show)

newtype Ptr = Ptr Word8 deriving (Eq, Show)
newtype Val = Val Word8 deriving (Eq, Show)
