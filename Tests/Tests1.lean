import LSpec
import RandomStock
import Parser
open LSpec

def sampleHalf := "Jiangxi Copper Co. Ltd. Class H      34,000    47,899        0.00"

def sampleCompany : CompanyInfo := { name := "Jiangxi Copper Co. Ltd. Class H", holding := 34000, marketValueGBP := 47899, percentOfTotal := 0.000000}

def main := do
  lspecIO $
    test "test alpha" (getParsed (Parser.run parseCommaNat "123456,987")== some 123) $
    test "test beta" ((getParsed (Parser.run compInfo sampleHalf))== some sampleCompany) $
    test "test gamma" ((getParsed (Parser.run lineHalf sampleHalf))== some (some sampleCompany))
