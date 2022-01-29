import Prettify
import QC
import Test.QuickCheck
import Control.Monad (forM_)

-- options = TestOptions {
--     no_of_tests = 200,
--     length_of_tests = 1,
--     debug_tests = False
-- }

-- Additional code for updated version (until main). Book's version is outdated
options = stdArgs { maxSuccess = 200, maxSize = 200}

type Run = Args -> IO ()

run :: Testable prop => prop -> Run 
run = flip quickCheckWith

runTests :: String -> Args -> [Run] -> IO ()
runTests name opts tests = putStrLn ("Running " ++ name ++ " tests:") >> forM_ tests (\rn -> rn opts)

main = do 
    runTests "simple" options 
        [ run prop_empty_id,
          run prop_char,
          run prop_text,
          run prop_line,
          run prop_double
        ]

    runTests "complex" options
        [ run prop_hcat,
          run prop_punctuate',
          run prop_mempty_id
        ]