{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoSimTypes where

---------------------------
---- IMPORTS --------------
---------------------------

-- Haskell  
import Prelude
import Data.List
import Data.Maybe

-- FFI
import Foreign 

--------------------------------------
---- TYPES ---------------------------
-------------------------------------- 

type SignalWords            = [Int32]
type SignalStream           = [SignalWords]

--------------------------------------
---- SUPPORTED TYPES - CoSimPack -----
-------------------------------------- 

class CoSimWords t where

    wordPack                :: t -> SignalWords
    wordUnpack              :: SignalWords -> t
    
instance {-# OVERLAPPABLE #-} (Integral a, Bits a) => CoSimWords a where

    wordPack x                
        | isJust size       = snd $ mapAccumR pack' x [0 .. wordSize]
        | otherwise         = error "Value does not have a fixed bitsize"
        where   
            size            = bitSizeMaybe x
            wordSize        = shiftR (fromJust size - 1) 5
            pack'           = \x _ -> (shiftR x 32, fromIntegral x)
    
    wordUnpack              = Prelude.foldl unpack' 0
        where
            unpack'         = \x y -> (shiftL x 32) .|. (maxInt32 .&. (fromIntegral y))
            maxInt32        = 4294967295


instance CoSimWords Bool where

    wordPack True           = [1]
    wordPack False          = [0]

    wordUnpack [x]          = not (x == 0)
    wordUnpack xs           = error $ "Cannot convert " ++ (show xs) ++ " to bool"

--------------------------------------
---- SUPPORTED TYPES - STREAM --------
--------------------------------------
              
class CoSimStream t where

    toSignalStream          :: t -> SignalStream
    fromSignalStream        :: SignalStream -> t

instance {-# OVERLAPPABLE #-} CoSimWords a => CoSimStream a where

    toSignalStream          = (:[]) . wordPack
    fromSignalStream        = wordUnpack . Prelude.head

instance CoSimWords a => CoSimStream [a] where

    toSignalStream          = Prelude.map wordPack
    fromSignalStream        = Prelude.map wordUnpack









