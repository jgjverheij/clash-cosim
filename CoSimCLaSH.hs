{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoSimCLaSH
    ( CoSimulator(..)
      ,coSim
      ,coSimCleanUp
      ,coSimDisableStdOut
      ,coSimEnableStdOut
      ,coSimSeq
      ,coSimWithFiles
      ,verilog
      ,wordPack
      ,wordUnpack
      ,mapAccumLM
    ) where

---------------------------
---- IMPORTS --------------
---------------------------

import CoSimTypes
import CoSimTypesExt

-- Haskell  
import Prelude
import Data.List
import Data.Maybe

-- FFI
import Foreign 
import Foreign.C

-- GC / IO / Monad
import System.Mem
import System.IO
import System.IO.Unsafe
import Control.Monad

-- Inline
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

--------------------------------------
---- FFI Imports ---------------------
--------------------------------------

foreign import ccall "simStart"         c_simStart      :: Ptr CInt -> CString -> Ptr CString -> IO (Ptr a)
foreign import ccall "&simEnd"          c_simEnd        :: FunPtr (Ptr a -> IO ())
foreign import ccall "simStep"          c_simStep       :: Ptr a -> IO CInt

foreign import ccall "getInputLength"   c_inputLength   :: Ptr a -> IO CInt
foreign import ccall "getOutputLength"  c_outputLength  :: Ptr a -> IO CInt
foreign import ccall "getInputSizes"    c_inputSizes    :: Ptr a -> IO (Ptr CInt)
foreign import ccall "getOutputSizes"   c_outputSizes   :: Ptr a -> IO (Ptr CInt)
foreign import ccall "getInputPtr"      c_inputPtr      :: Ptr a -> IO (Ptr (Ptr CInt))
foreign import ccall "getOutputPtr"     c_outputPtr     :: Ptr a -> IO (Ptr (Ptr CInt))

foreign import ccall "writeToFile"      c_writeToFile   :: CString -> IO CString

--------------------------------------
---- Types ---------------------------
--------------------------------------

-- (HDL, Period, ResetFase, Data, Files, Enable StdOut)
type CoSimSettings          = (Int, Int, Bool, String, [String], Bool)
type CoSimSettings'         = (CoSimSettings, CoSimulator, String)

data CoSimulator            = Icarus | ModelSim deriving (Show, Eq)

--------------------------------------
---- INLINE --------------------------
--------------------------------------

verilog                     :: QuasiQuoter
verilog                     = createQuasiQuoter' $ inlineCoSim' Icarus

inlineCoSim'                :: CoSimulator -> String -> Q Exp
inlineCoSim' hdl s          = liftM TupE $ sequence [q_hdl, q_period, q_reset, q_data, q_list, q_stdOut]
    where
        q_hdl               = lift (sim2Num hdl :: Int)
        q_period            = lift (20 :: Int)
        q_reset             = lift False
        q_data              = lift s
        q_list              = lift ([] :: [String])
        q_stdOut            = lift True

createQuasiQuoter'          :: (String -> Q Exp) -> QuasiQuoter        
createQuasiQuoter' f        = QuasiQuoter
                                {quoteExp  = f
                                ,quotePat  = undefined
                                ,quoteType = undefined
                                ,quoteDec  = undefined}

--------------------------------------
---- Help-Functions ------------------
--------------------------------------

sim2Num Icarus              = fromIntegral 1
sim2Num ModelSim            = fromIntegral 2

transposeList ::(Eq a, Num b, Eq b) => b -> [[a]] -> [[a]]
transposeList 0 _           = []
transposeList n xss         = ys : transposeList (n-1) yss
    where 
        ys                  = map head xss
        yss                 = map tail xss

mapAccumLM :: (acc -> x -> IO (acc, y)) -> acc -> [x] -> IO (acc, [y])        
mapAccumLM f s xs           = return $ mapAccumL (\a xs -> unsafePerformIO $ f a xs) s xs
  
--------------------------------------
---- Array Marshalling ---------------
--------------------------------------  
            
peekArray' :: (Storable a, Integral b) => b -> Ptr a -> IO [a]
peekArray' (-1) _           = error "null-pointer"
peekArray' size ptr         
    | ptr == nullPtr        = error "null-pointer"
    | otherwise             = peekArray (fromIntegral size) ptr
    
pokeArray' :: Storable a => Ptr a -> [a] -> IO ()
pokeArray' ptr []           = return ()
pokeArray' ptr xs 
    | ptr == nullPtr        = error "null-pointer"
    | otherwise             = pokeArray ptr xs

--------------------------------------
---- Co-Simulation -------------------
--------------------------------------

coSimCleanUp :: IO ()
coSimCleanUp                = performGC

coSimEnableStdOut :: CoSimSettings -> CoSimSettings
coSimEnableStdOut settings  = (a,b,c,d,e,True)
    where (a,b,c,d,e,_)     = settings

coSimDisableStdOut :: CoSimSettings -> CoSimSettings
coSimDisableStdOut settings = (a,b,c,d,e,False)
    where (a,b,c,d,e,_)     = settings

coSimWithFiles :: CoSimSettings -> [String] -> CoSimSettings
coSimWithFiles settings fs  = (a,b,c,d,(++) e fs,f)
    where (a,b,c,d,e,f)     = settings

coSimSeq :: CoSimSettings -> (Int,Bool) -> [String] -> CoSimSettings
coSimSeq set (p,rst) fs'    = (hdl, fromIntegral p, rst, m, (++) fs fs', stdOut)
        where 
            (hdl, _, _, m, fs, stdOut) = set 

coSim :: (CoSim r) => CoSimSettings -> CoSimulator -> String -> r
coSim settings sim top      = coSim' (settings, sim, top) False []

--------------------------------------
---- Co-Simulation MARSHALLING -------
--------------------------------------

coSimMarshall :: CoSimSettings' -> [SignalStream] -> IO (Bool, Ptr CInt, CString, Ptr CString)
coSimMarshall settings xs = do
        
        -- files
        c_f                 <- (newCString m) >>= c_writeToFile
        c_fs                <- mapM newCString d
        let c_files         = c_f : c_fs
        c_filePtrs          <- newArray c_files
        
        -- topEntity & settings
        c_topEntity         <- newCString top
        c_settingsPtr       <- newArray $ map fromIntegral $ c_settingsf c_files
        
        -- return
        return (c, c_settingsPtr, c_topEntity, c_filePtrs)
        
    where
        (set,sim,top)       = settings
        (a, b, c, m, d, e)  = set --(HDL, Period, ResetFase, Data, Files, Enable StdOut)
        c_settingsf fs      = [sim2Num sim, a, b, rst, stdOut, lenf fs, lenf xs]
        lenf                = length
        rst                 = if c then 1 else 0
        stdOut              = if e then 1 else 0

--------------------------------------
---- Co-Simulation START -------------
--------------------------------------

coSimStart :: CoSimSettings' -> [SignalStream] -> [SignalStream]
coSimStart settings xs  = unsafePerformIO $ do
    
        -- clean up
        coSimCleanUp   
        
        -- marshall c-types
        (rst, c_sPtr, c_topE, c_fPtrs) <- coSimMarshall settings xs
        
        -- start simulation
        c_coSimState'       <- c_simStart c_sPtr c_topE c_fPtrs
        when (c_coSimState' == nullPtr) $ error "Start co-simulation failed"

        -- add finilizer
        c_coSimState        <- newForeignPtr c_simEnd c_coSimState'
        
        -- perform simulation steps
        c_oLength           <- withForeignPtr c_coSimState c_outputLength 
        (_, ys)             <- mapAccumLM coSimStep c_coSimState $ f rst xs
        
        -- transpose and return
        return $ transposeList c_oLength ys
    where
        f r | r             = ([]:) . transpose
            | otherwise     = transpose

--------------------------------------
---- Co-Simulation STEP --------------
--------------------------------------

coSimStep :: ForeignPtr a -> SignalStream -> IO (ForeignPtr a, SignalStream)
coSimStep state xs          = do
        
        -- write input
        coSimInput state xs
        
        -- perform simulation step
        rv                  <- withForeignPtr state c_simStep
        when (rv /= 0) $ error "Error in co-simulation step"
                
        -- read output 
        ys                  <- coSimOutput state
        
        -- touch state, to keep state alive
        touchForeignPtr state
            
        -- return output    
        return (state, ys)
        
--------------------------------------
---- Co-Simulation INPUT OUTPUT ------
--------------------------------------
        
coSimInput :: ForeignPtr a -> SignalStream -> IO ()
coSimInput state xs         = do

    -- check sizes
    c_iLength               <- withForeignPtr state c_inputLength 
    c_iSizes                <- withForeignPtr state c_inputSizes >>= peekArray' c_iLength
    when (f c_iSizes) $ error $ errStr c_iSizes
    
    -- write input
    c_inputPtrs             <- withForeignPtr state c_inputPtr >>= peekArray' c_iLength
    zipWithM_ pokeArray' c_inputPtrs xs'
    
    where
        xs'                 = map (map fromIntegral) xs
        f                   = not . and . zipWith (\x y -> ( x == 0 ) || ( x == y )) iSizes
        iLength             = fromIntegral $ length xs
        iSizes              = map (fromIntegral . length) xs
        errStr ls           = concat ["Simulator expects ", show ls, " input words, but ", show iSizes, " given"]
        
        
coSimOutput :: ForeignPtr a -> IO SignalStream
coSimOutput state          = do

    -- read output   
    c_oLength               <- withForeignPtr state c_outputLength      
    c_oSizes                <- withForeignPtr state c_outputSizes   >>= peekArray' c_oLength
    c_outputPtrs            <- withForeignPtr state c_outputPtr     >>= peekArray' c_oLength
    ys                      <- zipWithM peekArray' c_oSizes c_outputPtrs
    
    -- convert and return
    return $ map (map fromIntegral) ys

--------------------------------------
---- PARSING -------------------------
--------------------------------------

parseInput :: CoSimStream t => [SignalStream] -> t -> [SignalStream]
parseInput xs x                 = toSignalStream x : xs

parseOutput :: CoSimStream t => ([SignalStream] -> [SignalStream]) -> Bool -> [SignalStream] -> ([SignalStream], t)
parseOutput f u xs           
        | qs == []              = error "Simulator expects less output ports"
        | otherwise             = (ys, fromSignalStream y)
        where 
            (y:ys)              = qs
            qs      | u         = xs
                    | otherwise = f $ reverse xs
                                
--------------------------------------
---- POLYVARIDIC ---------------------
--------------------------------------        

class CoSim r where

--------------------------------------
---- Func Definitions ----------------
--------------------------------------

    coSim' :: CoSimSettings' -> Bool -> [SignalStream] -> r

--------------------------------------
---- Instances Definitions -----------
--------------------------------------

instance {-# OVERLAPPABLE #-} CoSimStream t => CoSim t where
    coSim' s u xs           
        | ys == []              = y'
        | otherwise             = error "Simulator expects more output ports"
        where (ys, y')          = parseOutput (coSimStart s) u xs
             
instance {-# OVERLAPPING #-} (CoSimStream t, CoSim r) => CoSim (t, r) where
    coSim' s u xs               = (y', y'')
        where 
            (ys, y')            = parseOutput (coSimStart s) u xs
            y''                 = coSim' s True ys
                    
instance {-# OVERLAPPING #-} (CoSimStream t, CoSim r) => CoSim (t -> r) where
    coSim' s u xs               = coSim' s u . parseInput xs
           
--------------------------------------
---- Tupple Definitions --------------
--------------------------------------
            
instance {-# OVERLAPPING #-} CoSim (a,(b,r)) => CoSim (a,b,r) where
    coSim' s u                  = (\(a,(b,r)) -> (a,b,r)) . coSim' s u
        
instance {-# OVERLAPPING #-} CoSim (a,(b,c,r)) => CoSim (a,b,c,r) where
    coSim' s u                  = (\(a,(b,c,r)) -> (a,b,c,r)) . coSim' s u
            
instance {-# OVERLAPPING #-} CoSim (a,(b,c,d,r)) => CoSim (a,b,c,d,r) where
    coSim' s u                  = (\(a,(b,c,d,r)) -> (a,b,c,d,r)) . coSim' s u












