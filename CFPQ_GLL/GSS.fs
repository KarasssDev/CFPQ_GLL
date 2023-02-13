module CFPQ_GLL.GSS
open System.Collections.Generic
open CFPQ_GLL.Common

type GSSEdge (gssVertex: IGssVertex, rsmState: RsmState, matchedRange: MatchedRangeWithNode) =
    interface IGssEdge with
        member this.GssVertex = gssVertex
        member this.RsmState = rsmState
        member this.MatchedRange = matchedRange

and GssVertex (inputPosition: LinearInputGraphVertexBase, rsmState: RsmState, minWeightOfLeftPart:int<weight>) =
    let mutable minimalWeightOfLeftPart = minWeightOfLeftPart
    let outgoingEdges = ResizeArray<IGssEdge>()
    let popped = ResizeArray<MatchedRangeWithNode>()
    let handledDescriptors = HashSet<Descriptor>()
    interface IGssVertex with
        member this.MinimalWeightOfLeftPart
            with get () = minimalWeightOfLeftPart
            and set v = minimalWeightOfLeftPart <- v
        member this.InputPosition = inputPosition
        member this.RsmState = rsmState
        member this.OutgoingEdges = outgoingEdges
        member this.Popped = popped
        member this.HandledDescriptors = handledDescriptors

[<Struct>]
type GssVertexId =
    val InputPosition: LinearInputGraphVertexBase
    val RsmState: RsmState
    new(inputPosition, rsmState) = {InputPosition = inputPosition; RsmState = rsmState}

type GSS () =
    let vertices = Dictionary<GssVertexId, GssVertex>()
    member this.AddNewVertex (inputPosition: LinearInputGraphVertexBase, rsmState:RsmState, weight:int<weight>) =
        let gssVertexId = GssVertexId(inputPosition, rsmState)
        let exists, gssVertex = vertices.TryGetValue gssVertexId
        if exists
        then
            let _gssVertex = (gssVertex :> IGssVertex) 
            if _gssVertex.MinimalWeightOfLeftPart > weight
            then _gssVertex.MinimalWeightOfLeftPart <- weight
            gssVertex
        else
            let gssVertex = GssVertex(inputPosition, rsmState, weight)
            vertices.Add(gssVertexId, gssVertex)
            gssVertex

    member this.AddEdge (currentGSSVertex: IGssVertex
                         , rsmStateToReturn: RsmState
                         , inputPositionToContinue: LinearInputGraphVertexBase
                         , rsmStateToContinue: RsmState
                         , matchedRange: MatchedRangeWithNode) =
        let weight =
            match matchedRange.Node with
            | Some n -> n.Weight
            | None -> 0<weight>
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue, currentGSSVertex.MinimalWeightOfLeftPart + weight) :> IGssVertex
        let newEdge = GSSEdge(currentGSSVertex, rsmStateToReturn, matchedRange)

        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments.
        // Thus no check for duplicate GSS edges is needed."
        newGSSVertex.OutgoingEdges.Add newEdge
        newGSSVertex, newGSSVertex.Popped

    member this.Pop (currentDescriptor:Descriptor, matchedRange) =
        currentDescriptor.GssVertex.Popped.Add matchedRange
        currentDescriptor.GssVertex.OutgoingEdges

    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor) =
        let exists, handledDescriptor = descriptor.GssVertex.HandledDescriptors.TryGetValue descriptor
        let result = exists && handledDescriptor.Weight <= descriptor.Weight
        //if exists && handledDescriptor.Weight > descriptor.Weight then handledDescriptor.Weight <- descriptor.Weight
        result

    member this.AddDescriptorToHandled (descriptor:Descriptor) =
        descriptor.InputPosition.Descriptors.Add descriptor
        descriptor.GssVertex.HandledDescriptors.Add descriptor
        |> ignore
