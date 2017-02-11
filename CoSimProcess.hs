{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoSimProcess where

import System.Exit
import GHC.IO.Handle
import GHC.IO.Handle.FD (fdToHandle)
import System.Process
import Control.Monad


data CoSimulator                = CoSimulator [String] [String] deriving (Show) 

icarus top name fs              = CoSimulator commandCompile commandRun
    where
        commandCompile          = ["iverilog", "-s", top, "-o", name] ++ fs
        commandRun              = ["vvp", "-N", "-M.", "-mcosim_vpi", name]
   
startCoSimProcess args          = do

    

    _                           <- startProcessWait bcs
    (hRead, hWrite)             <- startProcess rcs
    
    print "hoi"
    where 
        (CoSimulator bcs rcs)   = args




------------------------------------

startProcessWait args           = do
    
        (_, _, _, h)            <- createProcess $ proc (head args) (tail args)
        exitCode                <- waitForProcess h
        return $ case exitCode of
                ExitSuccess     -> Nothing
                _               -> error "An error occured"
        
startProcess args               = do 
        
        -- create file-descripters
        (fd1, fd2)              <- createPipeFd
        (fd3, fd4)              <- createPipeFd
        
        -- start process
        (_, _, _, h)            <- createProcess (proc (head args) (tail args)) { env = Just 
                                                                [ ("fd1", show fd1)
                                                                , ("fd2", show fd2)
                                                                , ("fd3", show fd3)
                                                                , ("fd4", show fd4)] }
        -- create handlers
        [h1, h2, h3, h4]        <- mapM fdToHandle [fd1, fd2, fd3, fd4]
        
        -- close not used file-descriptors
        mapM_ hClose [h2, h3]

        -- return read & write handlers
        return (h1, h4)

    
    
