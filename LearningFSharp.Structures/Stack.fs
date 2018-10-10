namespace LearningFSharp.Structures

type Stack<'a> =
    | Empty
    | Node of 'a * Stack<'a>