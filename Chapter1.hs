module Chapter1 (Person(..), doubleAge, reverse_) where 
import Data.Function ((&))

data Person = 
    Person { name :: String
           , age :: Int }
    deriving (Eq, Show)

reverse_ :: [a] -> [a] 
reverse_ s = 
    let 
        aux c acc =
            case c of
            [] -> acc
            (h:t) -> h : aux t acc
    in aux s []

doubleAge p = Person {age = age p * 2, name = name p}