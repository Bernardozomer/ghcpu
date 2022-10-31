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
