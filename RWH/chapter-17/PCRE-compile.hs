type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !ByteString deriving (Eq, Show, Ord)

compile :: ByteString -> [PCREOption] -> Either String Regex 
compile str flags = unsafePerformIO $
    useASCString str $ \pattern -> do 
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
