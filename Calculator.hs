module Calculator where
import Data.Semigroup (Min(Min))
import Data.Char (isDigit)
import OUtils((|>))
import Data.Monoid

data Token = 
    Plus 
    | Minus
    | Numeric Int
    | Div
    | Mul 
    deriving (Show, Eq)

parseNum :: String -> Either String (Token, String) 
parseNum s = 
    let
        aux c acc =
            case c of 
                [] -> Right (acc, [])
                h:t | isDigit h -> aux t (h : acc) 
                remaining -> Right (acc, remaining)
    in aux s [] >>= \(res, rem) -> 
        Right (Numeric (read (reverse res) :: Int), rem)
tokenize :: String  -> Either String [Token]
tokenize s = 
    let 
        aux c acc = 
            case c of 
                [] -> Right (acc, [])
                ' ' : t -> aux t acc
                '-' : t ->  aux t (Minus : acc)
                '/' : t ->  aux t (Div : acc)
                '*' : t ->  aux t (Mul : acc)
                '+' : t ->  aux t (Plus : acc)
                full@(h : t) | isDigit h -> 
                    parseNum full  >>= \(res, rem) -> aux rem (res : acc)
                x -> let _ =  print x in Left "Ouch"
    in aux (s |> filter (/= ' ')) [] >>= \(res, shouldBeEmpty) -> Right $ reverse res
