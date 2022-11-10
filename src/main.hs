import Data.Word

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

data CPUState = CPUState Op Regs

data Op =
	  LoadInstr
	| ExeInstr Instr
	| ReadMem Ptr
	| WriteMem Ptr

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

newtype Ptr = Ptr Word8 deriving (Show)
newtype Val = Val Word8 deriving (Show)

-- Parse an instruction encoded in two Word8's.
parseInstr :: (Word8, Word8) -> Instr
parseInstr  (2, y) = Lod (Ptr y)
parseInstr  (4, y) = Sto (Ptr y)
parseInstr  (6, y) = Jmp (Ptr y)
parseInstr  (8, y) = Jmz (Ptr y)
parseInstr (10, y) = Cpe (Ptr y)
parseInstr (14, y) = Add (Ptr y)
parseInstr (16, y) = Sub (Ptr y)
parseInstr (18, y) = Nop
parseInstr (20, y) = Hlt

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

cycle :: (CPUState, RAM) ->  (CPUState, RAM)
cycle (CPUState op regs, RAM ram) = case op of
	ExeInstr instr -> case instr of
		Lod ptr -> (CPUState (ReadMem ptr) regs, RAM ram)
		Sto ptr -> (CPUState (WriteMem ptr) regs, RAM ram)
		Jmp ptr -> (CPUState LoadInstr regs { regIC = ptr }, RAM ram)
		Jmz ptr -> case readRegACC regs of
			Val 0     -> (CPUState LoadInstr regs { regIC = ptr }, RAM ram)
			otherwise -> (CPUState LoadInstr regs, RAM ram)
		Nop -> (CPUState op regs, RAM ram)
	ReadMem (Ptr ptr) -> (
			CPUState LoadInstr regs { regACC = ram !! (fromIntegral ptr) },
			RAM ram
		)
	WriteMem (Ptr ptr) -> (
			CPUState LoadInstr regs,
			RAM (setAt (fromIntegral ptr) (readRegACC regs) ram)
		)

-- Source: https://stackoverflow.com/a/5852820/7834359
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:setAt (n-1) newVal xs
