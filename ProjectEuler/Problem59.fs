module ProjectEuler.Problem59

#light
open System
open System.IO
open Combinations

let encryptedChars = 
    File.ReadAllText("cipher1.txt").Split(',') |> Array.map int
