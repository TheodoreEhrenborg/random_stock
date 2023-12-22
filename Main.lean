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

def sample_half := "Jiangxi Copper Co. Ltd. Class H      34,000    47,899        0.00"

def word : myParser (Array Char) := takeMany1 $ (Unicode.alpha <|> char '.')

#eval Parser.run word "helloHI there"
#eval Parser.run word "hi. there"
#eval Parser.run (char 'a': myParser Char) "123 there"

#eval Parser.run aaas "aaaa"
#eval Parser.run aaas "aaab"
#eval Parser.run aaas "aaba"
#eval Parser.run aaas "baaa"

-- So, each half of a line can either be:
-- - blank
-- - The name of a company
-- - [Comapany name/stock class]   Holding    Market Value in GBP   % of total net assets

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"
