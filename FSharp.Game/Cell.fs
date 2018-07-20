namespace FSharp.Game

open System

type Cell =

    member val Sym = ' ' with get, set
    member val ForegroundColor = ConsoleColor.Gray with get, set
    member val BackgroundColor = ConsoleColor.Black with get, set