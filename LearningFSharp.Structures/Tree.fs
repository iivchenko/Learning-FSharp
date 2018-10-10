namespace LearningFSharp.Structures

type Tree<'a> = 
    | Empty
    | Node of 'a * Tree<'a> * Tree<'a>