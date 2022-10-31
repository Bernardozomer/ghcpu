data Instr ad =
	  LOD ad
	| STO ad
	| JMP ad
	| JMZ ad
	| CPE ad
	| ADD ad
	| SUB ad
	| NOP
	| HLT
	deriving (Eq, Show)

data Reg =
	  ACC
	| EQZ
	| IC
	| IR
	| MAR
	| MDR
	| PC
	deriving (Show)
