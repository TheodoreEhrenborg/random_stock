import «RandomStock»
import Parser


/-
Copyright © 2022-2023 François G. Dorais, Kyrill Serdyuk, Emma Shroyer. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/


/-!
  Roman numeral parser
-/

namespace Roman

open Parser Char
/-- Roman parser monad -/
protected abbrev Parser := SimpleParser Substring Char

/-- Parse a roman numeral -/
protected def parse : Roman.Parser Nat := stepM >>= stepC >>= stepX >>= stepI
where

  stepM : Roman.Parser Nat :=
    (1000 * .) <$> countUpTo 3 (char 'M')

  stepC (n : Nat) : Roman.Parser Nat :=
    first [
      char 'C' *> char 'M' *> pure (n + 900),
      char 'C' *> char 'D' *> pure (n + 400),
      char 'D' *> (n + 500 + 100 * .) <$> countUpTo 3 (char 'C'),
      (n + 100 * .) <$> countUpTo 3 (char 'C')]

  stepX (n : Nat) : Roman.Parser Nat :=
    first [
      char 'X' *> char 'C' *> pure (n + 90),
      char 'X' *> char 'L' *> pure (n + 40),
      char 'L' *> (n + 50 + 10 * .) <$> countUpTo 3 (char 'X'),
      (n + 10 * .) <$> countUpTo 3 (char 'X')]

  stepI (n : Nat) : Roman.Parser Nat :=
    first [
      char 'I' *> char 'X' *> pure (n + 9),
      char 'I' *> char 'V' *> pure (n + 4),
      char 'V' *> (n + 5 + .) <$> countUpTo 3 (char 'I'),
      (n + .) <$> countUpTo 3 (char 'I')]

end Roman

/-- Read roman numeral from string -/
def String.toNatRoman? (s : String)  :=
  Parser.run (Roman.parse <* Parser.endOfInput) s.toSubstring
  --| .ok _ (n+1) => some (n+1)
  --| _ => none


#eval String.toNatRoman? "V"
#eval String.toNatRoman? "VI"
#eval Parser.run Roman.parse "XXV".toSubstring
#eval Parser.run Roman.parse "MMMM".toSubstring


namespace JSON

open Parser Char

/-- JSON parser monad -/
protected abbrev Parser := SimpleParser Substring Char

/-- Parse JSON white spaces -/
def ws : JSON.Parser Unit := dropMany <| tokenFilter fun c => c == ' ' || c == '\n' || c == '\r' || c == '\t'

/-- Parse a JSON number -/
protected partial def number : JSON.Parser Unit :=
  withErrorMessage "expected number" do
    let _ ← optional (char '-')
    match ← ASCII.digit with
    | ⟨0, _⟩ => pure ()
    | _ => dropMany ASCII.digit
    optional do
      let _ ← char '.'
      dropMany1 ASCII.digit
    optional do
      let _ ← char 'E' <|> char 'e'
      let _ ← optional (char '+' <|> char '-')
      dropMany1 ASCII.digit

end JSON

#eval Parser.run JSON.number "123.5".toSubstring

abbrev myParser := SimpleParser Substring Char

def x : myParser Char := Parser.Char.char '8'
def primes : myParser Char := Parser.first $ Parser.Char.char <$> ['2', '3', '5', '7']
def primes' : myParser Char := Parser.Char.char '2' <|> Parser.Char.char '3' --, '5', '7']

#eval Parser.run x '5'.toString
#eval Parser.run x '8'.toString

#eval Parser.run primes '8'.toString
#eval Parser.run primes '7'.toString
#eval Parser.run primes "77"
#eval Parser.run primes "58"
#eval Parser.run primes "85"
#eval Parser.run primes' "58"
#eval Parser.run primes' "28"
#eval Parser.run primes' "2"
#eval Parser.run (Parser.Char.ASCII.parseFloat: myParser Float) "85"
--#eval do
--  let foo : Option Float := match (Parser.run (Parser.Char.ASCII.parseFloat: myParser Float) "85") with
--    | Parser.Result.ok x y => some y
--    | _ => none
#eval do
  let x : Float <- match (Parser.run (Parser.Char.ASCII.parseFloat: myParser Float) "85.4e-1") with
    | Parser.Result.ok x y => some y
    | _ => none
  pure $ x + 10



open Parser
open Char

def aaas :myParser (Array Char) := takeMany1 $ char 'a'
#eval Parser.run aaas "aaaa"
#eval Parser.run aaas "aaab"
#eval Parser.run aaas "aaba"
#eval Parser.run aaas "baaa"

def sampleHalf := "Jiangxi Copper Co. Ltd. Class H      34,000    47,899        0.00"

#eval Array.foldl String.push "" #['a', 'b', 'c']
def digitCharParser : myParser Char := Nat.digitChar <$> Unicode.digit

def word : myParser String := (Array.foldl String.push "") <$>
    (takeMany1 $ (
    Unicode.alpha
    <|> digitCharParser
    <|> char '-'
    <|> char '&'
    <|> char '.'
    <|> char '\''
    <|> char '/'
    <|> char '('
    <|> char ')'
    <|> char '?'
    <|> char '%'
    <|> char ':'
    <|> char '+'
    <|> char '!'
    ))

#eval Parser.run word "helloHI there"
#eval Parser.run word "hi. there"
#eval Parser.run (char 'a': myParser Char) "123 there"
#eval Parser.run word sampleHalf

#eval "a" ++ "b"

def smartConcat (a: String) (sep: String) (b:String) : String :=
-- If a is empty, we don't want to use sep because
-- it'll create a sep at the beginning of the array
   match a with
   | "" => b
   | _ => a ++ sep ++ b

#eval smartConcat "one" "two" " "
#eval smartConcat "" "two" " "
#eval Array.foldl (smartConcat . " " . ) "" #["1","2","","3","", "", "4"]

def companyName : myParser String := (Array.foldl (smartConcat . " " . ) "") <$> (sepBy1 (char ' ') word)

#eval Parser.run companyName sampleHalf



def doubleDigit : myParser Nat := do
    let first <- Unicode.digit
    let second <- Unicode.digit
    pure $ 10*first + second

def tripleDigit : myParser Nat := do
    let first <- Unicode.digit
    let second <- Unicode.digit
    let third <- Unicode.digit
    pure $ 100*first + 10*second + third

#eval Parser.run tripleDigit "123"

def singleDigit : myParser Nat := Unicode.digit

def parseCommaNat : myParser Nat := do
   let firstChunk <- first [tripleDigit , doubleDigit ,singleDigit ]
   let tail <-  takeMany $ (char ',') *>  tripleDigit
   pure $ Array.foldl (.*1000+.) 0 $ #[firstChunk] ++ tail

def parseCommaNatOld : myParser Nat := (Array.foldl (.*1000+.) 0) <$> (sepBy1 (char ',') ASCII.parseNat)
-- TODO This is not perfect
-- It will incorrectly parse things like "1,23" as 1023 instead of rejecting them

#eval Parser.run parseCommaNat "123,456"
#eval Parser.run parseCommaNat "3,456"
#eval Parser.run parseCommaNat "3,46"
#eval Parser.run parseCommaNat "3"
#eval Parser.run parseCommaNat "34"
#eval Parser.run parseCommaNat "345"
#eval Parser.run parseCommaNat "345,"
#eval Parser.run parseCommaNat "123,46"
#eval Parser.run parseCommaNat "123,46,987"
#eval Parser.run parseCommaNat "123,456,987"
#eval Parser.run parseCommaNat "123456,987"


--#eval Parser.dropMany

structure CompanyInfo where
   name : String
   holding : Nat
   marketValueGBP : Nat
   percentOfTotal : Float
deriving Repr

instance : ToString CompanyInfo where
  toString c :=  s!"{c.name} Holding {c.holding} Market value (GBP) {c.marketValueGBP} Percent of total {c.percentOfTotal}"

def spaces : myParser (Array Char) := takeMany1 $ char ' '

def compInfo : myParser CompanyInfo := do
   let name <- companyName
   let _ <- spaces
   let holding <- parseCommaNat
   let _ <- spaces
   let marketValue <- parseCommaNat
   let _ <- spaces
   let percent <- ASCII.parseFloat
   pure { name := name , holding := holding, marketValueGBP := marketValue, percentOfTotal := percent : CompanyInfo}

#eval Parser.run compInfo sampleHalf

-- So, each half of a line can either be:
-- - blank
-- - The name of a company
-- - [Comapany name/stock class]   Holding    Market Value in GBP   % of total net assets
-- - We record the first two options as none

def nameHalf : myParser (Option CompanyInfo) := companyName *> pure none


def lineHalf : myParser (Option CompanyInfo) := optionD none (first [compInfo, nameHalf]) <* (takeMany $ char ' ')

#eval Parser.run lineHalf sampleHalf

-- How about:
-- Try to parse compInfo
-- If fails, try to parse just the name
-- If fails, just return none
-- Then do two of these, interspersed with at least 0 spaces
-- And then assert EOL

def fullLine : myParser (List (Option CompanyInfo)) := do
  let _ <- takeMany $ char ' '
  let first <- lineHalf
  let second <- lineHalf
  let _ <- eol
  pure [first, second]

#eval "foo"
#eval Parser.run fullLine "Dongfang Electric Corp. Ltd.     \n"
#eval Parser.run fullLine "                                                                          China Hongqiao Group Ltd.            61,500    47,871        0.00\n"
#eval Parser.run fullLine "   Class H                           55,000   61,764        0.00\n"
#eval Parser.run fullLine "China Conch Venture Holdings\n"
#eval Parser.run fullLine "   Ltd.                              48,000   60,617        0.00          Angelalign Technology Inc.            4,800    47,506        0.00\n"

#eval "4" ++ "\n"

def getResult (a : Result (Error.Simple Substring Char) Substring (List (Option CompanyInfo))) :=
  match a with
    | Parser.Result.ok x y => none
    | Parser.Result.error x => some x

def main (input: List String)  : IO UInt32 :=
  match input with
  | [] => pure 0
  | head :: rest => do
     let x := getResult ( Parser.run fullLine (head++"\n"))
     match x with
     | none => pure ()
     | some y => IO.println x
                 IO.println head
     main rest

-- The data I want starts on line 9259 and ends on line 14546

-- TODO Flaws:
-- Some entries are "-" (or some other dash), I guess because the company is worthless?
-- But then the company in the other column can't be parsed either
-- Similarly, each country has a row with total holdings, and that row makes its sibling
-- column unparseable
