import LSpec
import RandomStock
open LSpec

def fourIO : IO Nat :=
  return 4

def fiveIO : IO Nat :=
  return 5

def main := do
  let four ← fourIO
  lspecIO $
    test "fourIO equals 4" (four = 4) $
    test "4 equals 4" (4 = 4) $
    test "world is world" (hello = "world")
