multiply :: Int -> Int -> Int
multiply x y
    | y == 0    = 0
    | even y    = 2 * multiply x (y `div` 2)  -- if y is even
    | otherwise = x + 2 * multiply x (y `div` 2)  -- if y is odd

main :: IO ()
main = do
    let x = 6
    let y = 7
    putStrLn ("Multiplication of " ++ show x ++ " and " ++ show y ++ " is " ++ show (multiply x y))

-- If 0 return 0
-- If y is even, multple x by 2 then divide y by 2 then call multply recursively
-- If y is odd, add x and call multply recursively with x times 2 and (y-   1) / 2