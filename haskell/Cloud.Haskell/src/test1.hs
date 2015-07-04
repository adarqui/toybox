import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
 print "test1"
 Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
 node <- newLocalNode t initRemoteTable
 return ()
