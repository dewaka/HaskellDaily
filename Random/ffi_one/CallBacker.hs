module Main(main) where

import Foreign.C.Types(CDouble(..))
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

foreign import ccall "wrapper"
    wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))

foreign import ccall "callerback.h twice"
    twice :: FunPtr (CDouble -> CDouble) -> CDouble -> IO CDouble

square :: CDouble -> CDouble
square x = x * x

main :: IO ()
main = do
    squareW <- wrap square
    let x = 4
    y <- twice squareW x
    z <- twice squareW y
    print y
    print z
    freeHaskellFunPtr squareW

    


