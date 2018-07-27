namespace LearningFSharp.Games.GuessNumber

open System

module ConsoleModule =

    let cls ()=
        Console.Clear()
    
    let inline readLn () =
        Console.ReadLine()

    type Text =
    // think about STRING, Upper, lowwer text etc.
    | Gray of string
    | Green of string 
    | Yellow of string 
    | Red of string 
    | Line
    | Text of Text * Text
        static member (+) (left, right) =
            Text(left, right)

    let rec write (text:Text) =
        let origin = Console.ForegroundColor
        
        match text with
        | Gray(str) -> 
            Console.ForegroundColor <- ConsoleColor.Gray
            Console.Write(str)
        | Green(str) -> 
            Console.ForegroundColor <- ConsoleColor.Green
            Console.Write(str)
        | Yellow(str) -> 
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.Write(str)
        | Red(str) -> 
            Console.ForegroundColor <- ConsoleColor.Red
            Console.Write(str)
        | Line -> 
            Console.WriteLine()
        | Text(part1, part2) -> 
            write part1
            write part2

        Console.ForegroundColor <- origin

    let green text =
        Green(text)
    
    let yellow text = 
        Yellow(text)

    let gray text = 
        Gray(text)

    let red text = 
        Red(text)