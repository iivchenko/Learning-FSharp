namespace FSharp.Game

open System

type Grid(width:int, height:int) =
    
    member public this.Width = width
    member public this.Height = height

    member private this._matrix : Cell[,] = Array2D.zeroCreate this.Width this.Height
