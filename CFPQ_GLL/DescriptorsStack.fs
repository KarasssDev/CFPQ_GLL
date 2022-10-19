module CFPQ_GLL.DescriptorsStack

open System.Collections
open System.Collections.Generic
open CFPQ_GLL.Common
open CFPQ_GLL.RSM
open CFPQ_GLL.GSS
open CFPQ_GLL.InputGraph
open CFPQ_GLL.SPPF
open FSharpx.Collections

type IDescriptorsStack =
    abstract Push: Descriptor -> unit
    abstract Pop: unit -> Descriptor
    abstract IsEmpty: bool with get


type DefaultDescriptorsStack (seq: Descriptor seq) =
    let stack = Stack<_>(seq)
    new () = DefaultDescriptorsStack(Seq.empty)

    interface IDescriptorsStack with
        member this.Push descriptor = stack.Push descriptor
        member this.Pop () = stack.Pop ()
        member this.IsEmpty with get () = stack.Count = 0


type ErrorRecoveringDescriptorsStack () =
    let defaultDescriptorsStack = Stack<Descriptor>()
    let errorRecoveringDescriptorsStacks = SortedDictionary<int<distance>, Stack<Descriptor>>()

    interface IDescriptorsStack with
        member this.Push descriptor =
            let pathWeight =
                match descriptor.MatchedRange.Node with
                | Some node ->
                    
                    let weights =
                        let edges =
                            let r = ResizeArray<IGssEdge>()
                            descriptor.GssVertex :: [for e in descriptor.GssVertex.OutgoingEdges -> e.GssVertex]
                            |> List.iter (fun e ->r.AddRange e.OutgoingEdges)
                            r
                        descriptor.GssVertex.OutgoingEdges
                        //edges
                        |> ResizeArray.map (fun e -> 
                                               match e.MatchedRange.Node with
                                               | Some x -> x.Distance
                                               | None -> 0<distance>)
                        
                    //if weights.Count > 0 then weights |> Seq.min else 0<distance>
                    descriptor.LeftPartMinWeight + node.Distance
                | None -> descriptor.LeftPartMinWeight
            //printfn $"Descriptor weight: %A{pathWeight}"
            //printfn $"Keys:"
            //errorRecoveringDescriptorsStacks.Keys |> Seq.iter (fun x -> printf $"%A{x}; ")
            //printfn "\n"
            if pathWeight = 0<distance> then defaultDescriptorsStack.Push descriptor
            else
                if errorRecoveringDescriptorsStacks.ContainsKey(pathWeight) |> not then
                    errorRecoveringDescriptorsStacks[pathWeight] <- Stack<_>()
                errorRecoveringDescriptorsStacks[pathWeight].Push descriptor

        member this.Pop () =
            if defaultDescriptorsStack.Count > 0 then defaultDescriptorsStack.Pop ()
            else
                let mutable enumerator = errorRecoveringDescriptorsStacks.Keys.GetEnumerator()
                let moved = enumerator.MoveNext()
                assert moved
                let currentMin = enumerator.Current
                //printfn $"min = %A{currentMin}"
                let result = errorRecoveringDescriptorsStacks[currentMin].Pop ()
                if errorRecoveringDescriptorsStacks[currentMin].Count = 0 then
                    errorRecoveringDescriptorsStacks.Remove currentMin |> ignore
                result

        member this.IsEmpty
            with get () = defaultDescriptorsStack.Count = 0

