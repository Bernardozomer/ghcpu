import Data.Word

data Instr =
	  LOD Ptr
	| STO Ptr
	| JMP Ptr
	| JMZ Ptr
	| CPE Ptr
	| ADD Ptr
	| SUB Ptr
	| NOP
	| HLT
	deriving (Show)

newtype Ptr = Ptr Word8 deriving (Show)
newtype Val = Val Word8 deriving (Show)

data Reg =
	  ACC
	| EQZ
	| IC
	| IR
	| MAR
	| MDR
	| PC
	deriving (Show)
