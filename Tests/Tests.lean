import LSpec
import RandomStock
import Parser
open LSpec

def fourIO : IO Nat :=
  return 4

def fiveIO : IO Nat :=
  return 5

def sampleHalf := "Jiangxi Copper Co. Ltd. Class H      34,000    47,899        0.00"

def sampleCompany : CompanyInfo := { name := "Jiangxi Copper Co. Ltd. Class H", holding := 34000, marketValueGBP := 47899, percentOfTotal := 0.000000}



def main2 := do
  let four ← fourIO
  lspecIO $
    test "fourIO equals 4" (four = 4)

def main := do
  let four ← fourIO
  lspecIO $
    test "fourIO equals 4" (four = 4) $
    test "4 equals 4" (4 = 4) $
    test "test1" (getParsed (Parser.run word sampleHalf) == some "Jiangxi") $
    test "test2" (getParsed (Parser.run word "helloHI there") == some "helloHI") $
    test "test3" (getParsed (Parser.run word "hi. there") == some "hi.") $
    test "test4" (getParsed (Parser.run (Parser.Char.char 'a': myParser Char) "123 there") == none) $
    test "test5" ("a" ++ "b" == "ab") $
    test "test" (smartConcat "one" " " "two" == "one two") $
    test "test" (smartConcat "" " " "two" == "two") $
    test "test" (Array.foldl (smartConcat . " " . ) "" #["1","2","","3","", "", "4"] == "1 2  3   4") $
    test "test" (getParsed (Parser.run companyName sampleHalf) == some "Jiangxi Copper Co. Ltd. Class H") $
    test "test" (getParsed (Parser.run tripleDigit "123") == some 123) $
    test "test" (getParsed (Parser.run parseCommaNat "123,456") == some 123456) $
    test "test" (getParsed (Parser.run parseCommaNat "3,456")== some 3456) $
    test "test" (getParsed (Parser.run parseCommaNat "3,46")== some 3) $
    test "test" (getParsed (Parser.run parseCommaNat "3")== some 3) $
    test "test" (getParsed (Parser.run parseCommaNat "34")== some 34) $
    test "test" (getParsed (Parser.run parseCommaNat "345")== some 345) $
    test "test" (getParsed (Parser.run parseCommaNat "345,")== some 345) $
    test "test" (getParsed (Parser.run parseCommaNat "123,46")== some 123) $
    test "test" (getParsed (Parser.run parseCommaNat "123,46,987")== some 123) $
    test "test" (getParsed (Parser.run parseCommaNat "123,456,987")== some 123456987)
