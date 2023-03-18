module CFPQ_GLL.Input

open CFPQ_GLL.Common

type Range= {
    fromIndex: int
    toIndex: int
}

type IDynamicParser =
    abstract member Replace: Range -> string -> unit
    abstract member CurrentParseTree: SPPF.MatchedRanges with get
    abstract member CurrentInput: string with get

type IRope<'a> =
    inherit System.Collections.Generic.IEnumerable<'a>
    abstract member Insert: int -> 'a array -> unit
    abstract member Delete: int -> int -> unit
    abstract member Item: int -> 'a

type DynamicParser (initialInput: string, rope: unit -> IRope<LinearInputGraphVertexBase>) =

    let verticesMapping = System.Collections.Generic.Dictionary<int, int>()
    let mutable currentFreeIndex = 0

    let getVertices (input: string) =
        // TODO: fix me
        let terminals = input |> Seq.toArray |> Array.map Char
        let verticesCount = terminals.Length - 1
        let vertices = Array.init verticesCount  (fun i -> LinearInputGraphVertexBase(currentFreeIndex + i))
        for i in 1..terminals.Length - 1 do
            let fromVertex, toVertex, terminal = vertices[i], vertices[i + 1], terminals[i]
            fromVertex.AddOutgoingEdge (terminal, TerminalEdgeTarget(toVertex))
        currentFreeIndex <- currentFreeIndex + verticesCount
        // abcd ~~~ --a-> V1 --b-> V2 --c-> V3 --d->
        // terminals[0] = a, vertices = [V1, V2, V3], terminals[terminals.Length - 1] = d
        vertices, terminals[terminals.Length]

    let vertices =
        let finalVertex = LinearInputGraphVertexBase currentFreeIndex
        let vertices, terminal = getVertices initialInput
        (Array.last vertices).AddOutgoingEdge(terminal, TerminalEdgeTarget(finalVertex))
        let rope = rope()
        rope.Insert 0 (Array.append vertices [| finalVertex |])
        rope

    let delete (range: Range) =
        let beforeDeletedVertex = vertices[range.fromIndex]
        let afterDeletedVertex = vertices[range.toIndex + 1]
        let terminal, _ = vertices[range.toIndex].OutgoingEdge
        beforeDeletedVertex.RemoveOutgoingEdge()
        beforeDeletedVertex.AddOutgoingEdge(terminal, TerminalEdgeTarget(afterDeletedVertex))
        vertices.Delete (range.fromIndex + 1) (range.toIndex - range.fromIndex)

    let insert (index: int) (content: string) =
        // TODO: handle 0 and last indices
        let beforeInsertedVertex = vertices[index - 1]
        let afterInsertVertex = vertices[index]
        let insertedVertices, outgoingTerminal = getVertices content
        let incomingTerminal, _ = beforeInsertedVertex.OutgoingEdge
        beforeInsertedVertex.RemoveOutgoingEdge()
        beforeInsertedVertex.AddOutgoingEdge(incomingTerminal, TerminalEdgeTarget(insertedVertices[0]))
        (Array.last insertedVertices).AddOutgoingEdge(outgoingTerminal, TerminalEdgeTarget(afterInsertVertex))
        vertices.Insert index insertedVertices

    let updateParseTree () = ()

    let mutable currentParseTree = Unchecked.defaultof<SPPF.MatchedRanges>

    interface IDynamicParser with
        member this.CurrentInput with get () = failwith "todo"

        member this.CurrentParseTree with get () = currentParseTree

        member this.Replace range content =
            delete range
            insert range.fromIndex content
            updateParseTree ()
