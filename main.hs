module Main where
import Chapter1 (Person(..) , doubleAge, reverse_)
import OUtils ((|>))
import Calculator 
import Control.DeepSeq

sumUpTo :: Int -> Int
sumUpTo n = go 0 0
    where 
        -- This is enforcing strict eval. of acc (disable lazy mode)
        go !acc i
            | i > n = acc
            | otherwise = go (acc + i) (i + 1)

main :: IO () 
main = do
    -- print $ 
    --     let p = Person {name = "hi" , age = 300} 
    --     in iterate doubleAge p !! 15

    -- print $ sum [0..1000000000]
    -- print $  sumUpTo 100000000

    print $ tokenize  "90+11*1000"
