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
	regMDR :: Ptr,
	regPC  :: Ptr
} deriving (Show)

data Reg = RegACC | RegEQZ | RegIC | RegIR | RegMAR | RegMDR | RegPC
	deriving (Show)

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
