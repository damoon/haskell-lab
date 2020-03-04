import Test.HUnit
import ISBN2

main = runTestTT tests
tests = TestList [
  TestCase (assertEqual "valid isbn number"                                   True  (isValidISBN "3-598-21508-8")),
  TestCase (assertEqual "invalid isbn check digit"                            False (isValidISBN "3-598-21508-9")),
  TestCase (assertEqual "valid isbn number with a check digit of 10"          True  (isValidISBN "3-598-21507-X")),
  TestCase (assertEqual "check digit is a character other than X"             False (isValidISBN "3-598-21507-A")),
  TestCase (assertEqual "invalid character in isbn"                           False (isValidISBN "3-598-P1581-X")),
  TestCase (assertEqual "X is only valid as a check digit"                    False (isValidISBN "3-598-2X507-9")),
  TestCase (assertEqual "valid isbn without separating dashes"                True  (isValidISBN "3598215088")),
  TestCase (assertEqual "isbn without separating dashes and X as check digit" True  (isValidISBN "359821507X")),
  TestCase (assertEqual "isbn without check digit and dashes"                 False (isValidISBN "359821507")),
  TestCase (assertEqual "too long isbn and no dashes"                         False (isValidISBN "3598215078X")),
  TestCase (assertEqual "too short isbn"                                      False (isValidISBN "00")),
  TestCase (assertEqual "isbn without check digit"                            False (isValidISBN "3-598-21507")),
  TestCase (assertEqual "check digit of X should not be used for 0"           False (isValidISBN "3-598-21515-X")),
  TestCase (assertEqual "empty isbn"                                          False (isValidISBN "")),
  TestCase (assertEqual "input is 9 characters"                               False (isValidISBN "134456729")),
  TestCase (assertEqual "invalid characters are not ignored"                  False (isValidISBN "3132P34035")),
  TestCase (assertEqual "input is too long but contains a valid isbn"         False (isValidISBN "98245726788")), 
  TestCase (assertEqual "a valid isbn with a suffix"                          False (isValidISBN "9824572678A")) ]