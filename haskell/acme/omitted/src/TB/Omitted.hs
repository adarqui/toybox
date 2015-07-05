module TB.Omitted (
  omittedFunction,
  anotherOmittedFunction
) where

import           Acme.Omitted (omitted, (...))

omittedFunction :: a
omittedFunction = (...)

anotherOmittedFunction :: a
anotherOmittedFunction = omitted
