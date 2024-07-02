module OUtils  ((|>)) where
import Data.Function ((&))

(|>) :: a -> (a -> b) -> b
(|>) = (&)