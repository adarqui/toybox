import Pipes
import qualified Pipes.Prelude as P

import System.Environment (getArgs)

usage = "usage: ./stdinToStdout <index>"

r1 = runEffect $ for P.stdinLn (lift . putStrLn)
r2 = runEffect $ P.stdinLn >-> P.stdoutLn
r3 = runEffect $ lift getLine >~ P.stdoutLn
r4 = runEffect $ P.stdinLn >-> P.takeWhile (/= "quit") >-> P.stdoutLn

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  ("1":[]) -> r1
  ("2":[]) -> r2
  ("3":[]) -> r3
  ("4":[]) -> r4
  _ -> putStrLn usage
