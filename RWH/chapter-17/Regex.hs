{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

--  run with this
-- $ ghci -L/usr/local/lib/ -lpcre Regex.hs

-- test with
-- ghci > compile (pack "a.*b") [] -> return pointer


module Regex where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString.Char8 as B
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Unsafe
import Data.ByteString.Internal (toForeignPtr)



newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }

type PCREExtra = ()

--type PCREInfo = CInt

newtype PCREInfo = PCREInfo {unPCREInfo :: CInt} deriving (Eq,Show)

info_capturecount  :: PCREInfo
info_capturecount  = PCREInfo 2

{-# LINE 36 "Regex.hsc" #-}

caseless :: PCREOption
caseless = PCREOption 1
{-# LINE 39 "Regex.hsc" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 42 "Regex.hsc" #-}

dotall :: PCREOption
dotall = PCREOption 4
{-# LINE 45 "Regex.hsc" #-}


-- automatic binding with #enum
caseless2  :: PCREOption
caseless2  = PCREOption 1
dollar_endonly2  :: PCREOption
dollar_endonly2  = PCREOption 32
dotall2  :: PCREOption
dotall2  = PCREOption 4

{-# LINE 53 "Regex.hsc" #-}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0

type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !B.ByteString deriving (Eq, Show, Ord)

compile :: B.ByteString -> [PCREOption] -> Either String Regex 
compile str flags = unsafePerformIO $
    B.useAsCString str $ \pattern -> do 
        alloca $ \errptr -> do 
        alloca $ \erroffset -> do 
            pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
            if pcre_ptr == nullPtr
                then do 
                    err <- peekCString =<< peek errptr
                    return (Left err)
                else do 
                    reg <- newForeignPtr finalizerFree pcre_ptr -- free()
                    return (Right (Regex reg str))
foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec :: Ptr PCRE
                -> Ptr PCREExtra
                -> Ptr Word8
                -> CInt
                -> CInt
                -> PCREExecOption
                -> Ptr CInt
                -> CInt
                -> IO CInt

foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a 
                    -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do 
        c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
        return . fromIntegral =<< peek (n_ptr :: Ptr CInt)


match :: Regex -> B.ByteString -> [PCREExecOption] -> Maybe [B.ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)
    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off)
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)
            if r < 0
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> B.ByteString -> B.ByteString
    substring x y _ | x == y = B.empty
    substring a b s = end
        where
            start = unsafeDrop (fromIntegral a) s
            end   = unsafeTake (fromIntegral (b-a)) start