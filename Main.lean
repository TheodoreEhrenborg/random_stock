import RandomStock
import Parser
import LSpec


open Parser
open Char





#eval Parser.run fullLine "Dongfang Electric Corp. Ltd.     \n"
#eval Parser.run fullLine "                                                                          China Hongqiao Group Ltd.            61,500    47,871        0.00\n"
#eval Parser.run fullLine "   Class H                           55,000   61,764        0.00\n"
#eval Parser.run fullLine "China Conch Venture Holdings\n"
#eval Parser.run fullLine "   Ltd.                              48,000   60,617        0.00          Angelalign Technology Inc.            4,800    47,506        0.00\n"

def getError (a : Result (Error.Simple Substring Char) Substring (List (Option CompanyInfo))) :=
  match a with
    | Parser.Result.error x => some x
    | _ => none

def getResult (a : Result (Error.Simple Substring Char) Substring (List (Option CompanyInfo))) :=
  match a with
    | Parser.Result.ok _ y => some y
    | _ => none


def processLine (line: String):= getResult $ Parser.run fullLine (line++"\n")

def filterOutNone : List (Option α) -> List α
   | none::rest => filterOutNone rest
   | some x ::rest => x :: filterOutNone rest
   | [] => []

#eval filterOutNone ([none, some 3, none, some 4] : List (Option Nat))
#eval [none, some 3, none, some 4].filterMap id

def flatten : List (List α) -> (List α)
   | [] => []
   | []::rest => flatten rest
   | lyst::rest => lyst ++ flatten rest
#eval flatten [[1,2,3], [4,5], [], [6]]

partial def takeAtMost (n: Nat) (lyst: List α) : List α := match (n, lyst) with
   | (_, []) => []
   | (0, _) => []
   | (n, head::rest) => head::takeAtMost (n-1) rest

def observedPercentage (c: CompanyInfo) (total: Nat) : Float :=
    100.0 * c.marketValueGBP.toFloat / total.toFloat

def cryptoRandLessThan (n : Nat) : IO (Option Nat) := do
    let command := { cmd := "python" , args := #["-c" ,
        s!"import secrets; print(secrets.randbelow({n}))"] : IO.Process.SpawnArgs }
    let result <- IO.Process.run command
    pure $ (result.stripSuffix "\n").toNat?

#eval "344".stripSuffix "4"


def main2 (input: List String)  : IO UInt32 := do
    IO.println $ s!"{<-(cryptoRandLessThan 1000)}"
    IO.println $ s!"{<-(cryptoRandLessThan 1000)}"
    pure 0

def pickOutCompany ( cs : List CompanyInfo) (randomPound : Nat) : Option CompanyInfo :=
    match cs with
    | [] => none
    | candidate::rest => let candWorth := candidate.marketValueGBP
                          if randomPound < candWorth then
                            some candidate
                          else pickOutCompany rest (randomPound - candWorth)

def printCompany (totalGBP : Nat) ( cs: List CompanyInfo )  : IO Unit  := do
    let randomGBP <- cryptoRandLessThan totalGBP
    IO.println s!"Random company: {pickOutCompany cs <$> randomGBP}"


def main : IO Unit := do
    let lines <- IO.FS.lines "/home/theo/vanguard_playground/oeic-interim-long-report.txt"
    let trimmedLines := (lines.toSubarray 9258 14546).toArray.toList
    let initialData : List (Option (List (Option CompanyInfo))) := processLine <$> trimmedLines
    let notErrorData: List (List (Option CompanyInfo)) := filterOutNone initialData
    -- A row containing two entries now becomes two separate entries in the list
    let rowsAreSplit : List (Option CompanyInfo) := flatten notErrorData
    let companyInfos : List CompanyInfo := filterOutNone rowsAreSplit
    let sorted := companyInfos.toArray.qsort (fun x y=> x.marketValueGBP > y.marketValueGBP)
    IO.println s!"The total number of companies is {sorted.toList.length}"
    let totalGBP := List.foldr (fun company soFar => company.marketValueGBP + soFar) 0 companyInfos
    IO.println s!"The total worth of all companies is {totalGBP} GBP"
    let howMany := 20
    IO.println s!"The {howMany} biggest companies are"
    IO.println $ (fun c => (c, observedPercentage c totalGBP)) <$> sorted.toList.take howMany
    IO.println ""
    --IO.println s!"{pickOutCompany sorted.toList 83812713}"
    for i in [:1] do
        printCompany totalGBP sorted.toList

-- TODO Print the PDF's percentage and the calculated percentage to make sure they're very close

#eval IO.println "h\ti"
#eval Array.qsort #[1, 2, 3, -10, -20] (. > .)
#eval #[1, 2, 3, -10, -20].qsort (. > .)

def lookForErrors (input: List String)  : IO UInt32 :=
  match input with
  | [] => pure 0
  | head :: rest => do
     let x := getError ( Parser.run fullLine (head++"\n"))
     match x with
     | none => pure ()
     | some y => IO.println x
                 IO.println head
     lookForErrors rest
