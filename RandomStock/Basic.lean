import Parser

open Parser
open Char

abbrev myParser := SimpleParser Substring Char


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

def getParsed (a : Parser.Result (Parser.Error.Simple Substring Char) Substring (alpha)) : Option alpha :=
  match a with
    | Parser.Result.ok _ y => some y
    | _ => none

def smartConcat (a: String) (sep: String) (b:String) : String :=
-- If a is empty, we don't want to use sep because
-- it'll create a sep at the beginning of the array
   match a with
   | "" => b
   | _ => a ++ sep ++ b
def companyName : myParser String := (Array.foldl (smartConcat . " " . ) "") <$> (sepBy1 (char ' ') word)

def doubleDigit : myParser Nat := do
    let first <- Unicode.digit
    let second <- Unicode.digit
    pure $ 10*first + second

def tripleDigit : myParser Nat := do
    let first <- Unicode.digit
    let second <- Unicode.digit
    let third <- Unicode.digit
    pure $ 100*first + 10*second + third

def singleDigit : myParser Nat := Unicode.digit

def parseCommaNat : myParser Nat := do
   let firstChunk <- first [tripleDigit , doubleDigit ,singleDigit ]
   let tail <-  takeMany $ (char ',') *>  tripleDigit
   pure $ Array.foldl (.*1000+.) 0 $ #[firstChunk] ++ tail

structure CompanyInfo where
   name : String
   holding : Nat
   marketValueGBP : Nat
   percentOfTotal : Float
deriving Repr, BEq


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

-- So, each half of a line can either be:
-- - blank
-- - The name of a company
-- - [Comapany name/stock class]   Holding    Market Value in GBP   % of total net assets
-- - We record the first two options as none

def nameHalf : myParser (Option CompanyInfo) := companyName *> pure none


def lineHalf : myParser (Option CompanyInfo) := optionD none (first [compInfo, nameHalf]) <* (takeMany $ char ' ')


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
