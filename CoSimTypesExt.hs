{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoSimTypesExt where

---------------------------
---- IMPORTS --------------
---------------------------

-- Haskell  
import CoSimTypes

-- CLaSH
import CLaSH.Prelude
import CLaSH.Signal.Explicit

--------------------------------------
---- SUPPORTED TYPES - SINGLE --------
-------------------------------------- 

instance (    BitPack (rep (int + frac))
            , KnownNat (BitSize (rep (int + frac)))
            , KnownNat (BitSize (rep (int + frac)) + 1)
            , KnownNat (BitSize (rep (int + frac)) + 2)) => (CoSimWords (Fixed rep int frac)) where
            
    wordPack                = wordPack . pack
    wordUnpack              = unpack . wordUnpack

--------------------------------------
---- SUPPORTED TYPES - STREAM --------
--------------------------------------

instance CoSimWords a => CoSimStream (Signal' clk a) where

    toSignalStream          = toSignalStream . sample
    fromSignalStream        = fromList . fromSignalStream











