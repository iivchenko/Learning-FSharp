namespace LearningFSharp.Structures

type Tree<'T1> = 
    | Empty
    | Node of 'T1 * Tree<'T1> * Tree<'T1>