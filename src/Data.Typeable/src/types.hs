import Data.Int
import Data.Word
import Data.Ord
import Data.Typeable
import Control.Monad.ST
import Control.Exception
import Foreign.C.Types

main = do
    putStrLn $ show $ typeOf (True::Bool)
    putStrLn $ show $ typeOf ('c'::Char)
    putStrLn $ show $ typeOf (1.0::Double)
    putStrLn $ show $ typeOf (1.0::Float)
    putStrLn $ show $ typeOf (1::Int)
    putStrLn $ show $ typeOf (1::Int8)
    putStrLn $ show $ typeOf (1::Int16)
    putStrLn $ show $ typeOf (1::Int32)
    putStrLn $ show $ typeOf (1::Int64)
    putStrLn $ show $ typeOf (1::Integer)
    putStrLn $ show $ typeOf (undefined::Ordering)
    putStrLn $ show $ typeOf (undefined::RealWorld)
    putStrLn $ show $ typeOf (undefined::Word)
    putStrLn $ show $ typeOf (undefined::Word8)
    putStrLn $ show $ typeOf (undefined::Word16)
    putStrLn $ show $ typeOf (undefined::Word32)
    putStrLn $ show $ typeOf (undefined::Word64)
    putStrLn $ show $ typeOf (undefined::())
    putStrLn $ show $ typeOf (undefined::TyCon)
    putStrLn $ show $ typeOf (undefined::TypeRep)
    putStrLn $ show $ typeOf (undefined::ArithException)
    putStrLn $ show $ typeOf (undefined::ErrorCall)
    putStrLn $ show $ typeOf (undefined::SomeException)
    putStrLn $ show $ typeOf (undefined::IOException)
    putStrLn $ show $ typeOf (undefined::CUIntMax)
    putStrLn $ show $ typeOf (undefined::CIntMax)
    putStrLn $ show $ typeOf (undefined::CUIntPtr)
    putStrLn $ show $ typeOf (undefined::CIntPtr)

