namespace LearningFSharp.Structures

type Stack<'T> =
    | Empty
    | Node of 'T * Stack<'T>