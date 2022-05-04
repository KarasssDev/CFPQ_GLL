module CFPQ_GLL.GSS
open System.Collections.Generic
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open CFPQ_GLL.SPPF
open FSharpx.Collections

[<Measure>] type gssVertex
[<Measure>] type gssEdge
[<Measure>] type descriptorWithoutGSSVertex

[<Measure>] type gssEdgeContent

[<Struct>]
type GSSVertex =
    val InputPosition: int<graphVertex>
    val RSMState: int<rsmState>
    new (inputPosition, rsmState) = {InputPosition = inputPosition; RSMState = rsmState}

[<Struct>]
type Descriptor =
    val InputPosition: int<graphVertex>
    val GSSVertex: GSSVertex
    val RSMState: int<rsmState>
    val MatchedRange: Option<MatchedRange>
    new(inputPosition, gssVertex, rsmState, matchedRange) =
        {
            InputPosition = inputPosition
            GSSVertex = gssVertex
            RSMState = rsmState
            MatchedRange = matchedRange
        }

[<Struct>]
type GSSEdge =
    val GSSVertex : GSSVertex
    val RSMState : int<rsmState>
    new(gssVertex, rsmState) =
        {
            GSSVertex = gssVertex
            RSMState = rsmState
        }

// gssEdge = |gssVertex|rsmState|
// gssVertex = |...|InputPosition|rsmState|
let MASK_FOR_INPUT_POSITION = int64 (System.UInt64.MaxValue >>> BITS_FOR_GRAPH_VERTICES + BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_GSS_VERTEX = int64 (System.UInt64.MaxValue >>> BITS_FOR_RSM_STATE <<< BITS_FOR_RSM_STATE)
let MASK_FOR_RSM_STATE = int64 (System.UInt64.MaxValue >>> 2 * BITS_FOR_GRAPH_VERTICES)

let inline packDescriptorWithoutGSSVertex (inputPos:int<graphVertex>) (rsmState:int<rsmState>) : int64<descriptorWithoutGSSVertex>=
    let _inputPos = (int64 inputPos) <<< BITS_FOR_RSM_STATE    
    let _rsmState = int64 rsmState
    (_inputPos ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure
    
let packGSSEdge (targetGSSVertex:int64<gssVertex>) (rsmState:int<rsmState>) : int64<gssEdge> =
    let _targetGSSVertex = (int64 targetGSSVertex) <<< BITS_FOR_RSM_STATE
    let _rsmState = int64 rsmState
    (_targetGSSVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure

let packGSSVertex (gssVertex:GSSVertex) : int64<gssVertex> =
    let _targetGSSVertex = (int64 gssVertex.InputPosition) <<< BITS_FOR_RSM_STATE
    let _rsmState = int64 gssVertex.RSMState
    (_targetGSSVertex ||| _rsmState) |> LanguagePrimitives.Int64WithMeasure

let unpackGSSVertex (gssVertex:int64<gssVertex>) =
    let gssVertex = int64 gssVertex
    let inputPosition = int32 (gssVertex &&& MASK_FOR_GSS_VERTEX >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure    
    let rsmState = int32 (gssVertex &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    GSSVertex (inputPosition, rsmState)

let unpackGSSEdge (gssEdge:int64<gssEdge>) =
    let gssEdge = int64 gssEdge
    let gssVertex = int64 (gssEdge &&& MASK_FOR_GSS_VERTEX >>> BITS_FOR_RSM_STATE) |> LanguagePrimitives.Int64WithMeasure    
    let rsmState = int32 (gssEdge &&& MASK_FOR_RSM_STATE) |> LanguagePrimitives.Int32WithMeasure
    GSSEdge (unpackGSSVertex gssVertex, rsmState)
    
[<Struct>]
type GssVertexContent =
    val OutputEdges : ResizeArray<int64<gssEdge>>
    val Popped : ResizeArray<int<graphVertex>>
    val HandledDescriptors : HashSet<int64<descriptorWithoutGSSVertex>>
    new (outputEdges, popped, handledDescriptors) = {OutputEdges = outputEdges; Popped = popped; HandledDescriptors = handledDescriptors}

type GSS() =
    let vertices = Dictionary<int64<gssVertex>,GssVertexContent>()
    
    member this.AddNewVertex (inputPosition:int<graphVertex>, rsmState:int<rsmState>) =
        let gssVertex = GSSVertex(inputPosition, rsmState)
        let packedGSSVertex = packGSSVertex gssVertex
        if vertices.ContainsKey packedGSSVertex
        then gssVertex
        else
            vertices.Add(packedGSSVertex, GssVertexContent(ResizeArray<_>(),ResizeArray<_>(), HashSet<_>()))
            gssVertex
   
    member this.AddEdge (currentGSSVertex:GSSVertex, rsmStateToReturn:int<rsmState>, inputPositionToContinue:int<graphVertex>, rsmStateToContinue:int<rsmState>) =
        let newGSSVertex = this.AddNewVertex (inputPositionToContinue, rsmStateToContinue)
        let newGSSVertexContent = vertices.[packGSSVertex newGSSVertex]
        let newEdge = packGSSEdge (packGSSVertex currentGSSVertex) rsmStateToReturn
        
        // There is no need to check GSS edges duplication.
        // "Faster, Practical GLL Parsing", Ali Afroozeh and Anastasia Izmaylova
        // p.13: "There is at most one call to the create function with the same arguments. Thus no check for duplicate GSS edges is needed."
        newGSSVertexContent.OutputEdges.Add newEdge        
        newGSSVertex, newGSSVertexContent.Popped
        
    member this.Pop (currentDescriptor:Descriptor) =
        let gssVertexContent = vertices.[packGSSVertex currentDescriptor.GSSVertex]
        gssVertexContent.Popped.Add currentDescriptor.InputPosition
        gssVertexContent.OutputEdges
        |> ResizeArray.map unpackGSSEdge
        
    member this.IsThisDescriptorAlreadyHandled (descriptor:Descriptor) =
        packDescriptorWithoutGSSVertex descriptor.InputPosition descriptor.RSMState
        |> vertices.[packGSSVertex descriptor.GSSVertex].HandledDescriptors.Contains 
    
    member this.AddDescriptorToHandled (descriptor:Descriptor) =
        packDescriptorWithoutGSSVertex descriptor.InputPosition descriptor.RSMState
        |> vertices.[packGSSVertex descriptor.GSSVertex].HandledDescriptors.Add
        |> ignore