import System.Exit ( exitFailure, exitSuccess )
import System.Process ( readProcess )

main :: IO ()
main = do
    output <- readProcess "sh" ["testfile.sh", "try.spl"] []
    if output == "\ESC[32m\9989 Program is correctly typed\ESC[0m\n\n\ESC[36m0\ESC[0m\n\ESC[36m1\ESC[0m\n\ESC[36m2\ESC[0m\n\ESC[36m3\ESC[0m\n\ESC[36m4\ESC[0m\n\ESC[36m5\ESC[0m\n\ESC[36m6\ESC[0m\n\ESC[36m7\ESC[0m\n\ESC[36m8\ESC[0m\n\nmachine halted\n"
        then putStrLn "\x1b[32mTest succeeded\x1b[0m"
        else exitFailure
