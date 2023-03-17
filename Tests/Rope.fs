module Tests.Rope

type 'a Rope =
    | Node of {| Height: int; Length: int; Left: 'a Rope; Right: 'a Rope |}
    | Leaf of 'a array
    with
        member this.Height with get () =
            match this with
            | Node node -> node.Height
            | Leaf _ -> 0
        member this.Length with get () =
            match this with
            | Node node -> node.Length
            | Leaf content -> content.Length

let nil = Leaf [||]

let private union (left: 'a Rope) (right: 'a Rope) =
    if left = nil then right
    else if right = nil then left
    else
        Node {|
            Height = 1 + max left.Height right.Height
            Length = left.Length + right.Length
            Left = left
            Right = right
        |}

let rec private split (node: 'a Rope) (index: int) =
    match node with
    | Leaf content ->
        let leftContent, rightContent = Array.splitAt index content
        Leaf leftContent, Leaf rightContent
    | Node node ->
        if index < node.Left.Length then
            let newLeftNode, newRightNodeLeft = split node.Left index
            let newRightNode =
                Node {|
                    Height = 1 + max newRightNodeLeft.Height node.Right.Height
                    Length = newRightNodeLeft.Length + node.Right.Length
                    Left = newRightNodeLeft
                    Right = node.Right
                |}
            newLeftNode, newRightNode
        else
            let newRightNode, newLeftNodeRight = split node.Right (index - node.Left.Length)
            let newLeftNode =
                Node {|
                    Height = 1 + max newLeftNodeRight.Height node.Left.Height
                    Length = newLeftNodeRight.Length + node.Left.Length
                    Left = node.Left
                    Right = newLeftNodeRight
                |}
            newLeftNode, newRightNode

let private merge (left: 'a Rope) (right: 'a Rope) =
    union left right

let insert (node: 'a Rope) (index: int) (content: 'a array) =
    let newLeftLeft, newRight = split node index
    let newLeftRight = Leaf content
    let newLeft = merge newLeftLeft newLeftRight
    merge newLeft newRight

let delete (node: 'a Rope) (fromIndex: int) (toIndex: int) =
    let newRight, tmpNode = split node fromIndex
    let _, newLeft = split tmpNode (toIndex - fromIndex)
    merge newRight newLeft

let rec get (node: 'a Rope) (index: int) =
    match node with
    | Leaf content -> content[index]
    | Node node ->
        let leftNode = node.Left
        if leftNode.Length >= index then
            get leftNode index
        else
            get node.Right (index - leftNode.Length)

let rec getContent (node: 'a Rope) =
    match node with
    | Node node -> Array.concat [| getContent node.Left; getContent node.Right |]
    | Leaf content -> content

let contains (node: 'a Rope) (index: int) = index < node.Length

let getOperations (rope: 'a Rope ref) =
    let insert' index v = rope.Value <- insert rope.Value index v
    let delete' fromIndex toIndex = rope.Value <- delete rope.Value fromIndex toIndex
    insert', delete', get rope.Value

let empty = Leaf [||]
let fromArray = Leaf
