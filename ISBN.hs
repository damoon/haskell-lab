module ISBN where

import Data.Char

data ISBN = WrongLength | BadEncoding | InvalidCheck | ValidISBN String | DecodedISBN [Int] deriving Show

isValidISBN :: String -> Bool
isValidISBN chars = toBool $ checkValidity $ decode $ checkEncoding $ checkLength $ dropHyphen $ ValidISBN chars

checkLength :: ISBN -> ISBN
checkLength (ValidISBN a)
    | length a == 10 = ValidISBN a
    | otherwise = WrongLength
checkLength a = a

checkValidity :: ISBN -> ISBN
checkValidity (DecodedISBN [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]) =
    let s = x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1
        m = mod s 11
    in case m of 0 -> DecodedISBN [x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
                 _ -> InvalidCheck
checkValidity a = a

checkEncoding :: ISBN -> ISBN
checkEncoding (ValidISBN a) =
    let digitsOk = foldl (&&) True (map isDigit (take 9 a))
        checkDigit = last a
        checkDigitOk = isDigit checkDigit || checkDigit == 'x' || checkDigit == 'X'
        ok = digitsOk && checkDigitOk
    in case ok of True -> ValidISBN a
                  False -> BadEncoding
checkEncoding a = a

decode :: ISBN -> ISBN
decode (ValidISBN a) =
    DecodedISBN $ digits ++ [checkDigit]
    where digits = map digitToInt (take 9 a)
          checkDigit = decodeLast (last a)
decode a = a

decodeLast 'x' = 10
decodeLast 'X' = 10
decodeLast a = digitToInt a


toBool :: ISBN -> Bool
toBool (DecodedISBN _) = True
toBool _ = False

dropHyphen :: ISBN -> ISBN
dropHyphen (ValidISBN l) = ValidISBN $ filter (\a -> a /= '-') l
dropHyphen a = a
