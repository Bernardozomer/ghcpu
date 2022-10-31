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

data Reg = Reg {
	regACC :: Val,
	regEQZ :: Bool,
	regIC :: Ptr,
	regIR :: (Word8, Word8),
	regMAR :: Ptr,
	regMDR :: Ptr,
	regPC :: Ptr
} deriving (Show)
