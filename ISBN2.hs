module ISBN2 where

import Data.Char

isValidISBN :: String -> Bool
isValidISBN chars =
    let noHyphen = filter (/= '-') chars
        digits = map digitToInt (filter isDigit noHyphen)
        remainder = sum $ zipWith (*) [10,9 .. 1] digits
        checkDigit = last $ take 10 noHyphen
        ld = length digits
        lnh = length noHyphen
    in case (ld, lnh, checkDigit) of (9, 10, 'X') -> mod (remainder + 10) 11 == 0
                                     (10, 10, _)  -> mod remainder 11 == 0
                                     _ -> False
