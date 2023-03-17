module Benchmarks.BenchmarkData

open System.IO
open System.Text.RegularExpressions
open CFPQ_GLL
open CFPQ_GLL.Common
open FSharpx.Collections

let private seed = 100
let private rnd = System.Random(seed)
let workDirectory = "/home/viktor/Documents/code/work/CFPQ_GLL/golang"
let configPath = $"{workDirectory}{Path.DirectorySeparatorChar}config"

module DataGeneration =
    type SyntaxError =
        | KeywordError
        | BracketError
        | OperationError
        | DotCommaError
        | TruncatedBlockError
        | TruncatedLineError
        with
            static member toInt (e: SyntaxError) =
                match e with
                | KeywordError -> 1
                | BracketError -> 2
                | OperationError -> 3
                | DotCommaError -> 4
                | TruncatedBlockError -> 5
                | TruncatedLineError -> 6
            static member fromInt (i: int) =
                match i with
                | 1 -> KeywordError
                | 2 -> BracketError
                | 3 -> OperationError
                | 4 -> DotCommaError
                | 5 -> TruncatedBlockError
                | 6 -> TruncatedLineError
                | x -> failwith $"Invalid SyntaxError: {x}"
            static member fromString (s: string) =
                match s with
                | "Keyword" -> KeywordError
                | "Bracket" -> BracketError
                | "Operation" -> OperationError
                | "DotComma" -> DotCommaError
                | "TruncatedBlock" -> TruncatedBlockError
                | "TruncatedLine" -> TruncatedLineError
                | x -> failwith $"Invalid SyntaxError: {x}"


    let localErrors = [| KeywordError; BracketError; OperationError; DotCommaError |]
    let nonLocalErrors = [| TruncatedLineError; TruncatedBlockError |]
    let allErrors = Array.concat [localErrors; nonLocalErrors]
    
    type Config = {
        sizes: int array
        errorsCount: int array
        allowedErrors: SyntaxError array
    }
    with
        static member parse (s: string) =
            let lines = s.Split "\n"
            let sizes = lines[0].Split " " |> Seq.map int |> Array.ofSeq
            let errors = lines[1].Split " " |> seq |> Seq.map int |> Array.ofSeq
            let allowedErrors =
                match lines[2].Split " " with
                | x when x[0] = "All" -> allErrors
                | x when x[0] = "Local" -> localErrors
                | x when x[0] = "NonLocal" -> nonLocalErrors
                | x -> x |> Array.map SyntaxError.fromString
            {
                sizes = sizes
                errorsCount = errors
                allowedErrors = allowedErrors
            }

    let private keywords = [| "if"; "else"; "true"; "false"; "return"; "bool"; "int"; "func"|]
    let private brackets = [| "\("; "\)"; "\{"; "\}" |]
    let private operations = [| "\+"; "-"; "\*"; "\^"; "="; "<"; ">"; "<="; ">="; "!="; "&&"; "||" |]

    let private breakKeyword (keyword: string) =
        let index = rnd.Next(0, keyword.Length)
        printfn $"{keyword} {keyword.Length}"
        match rnd.Next 2 with
        | 0 ->
            printfn $"{keyword} {keyword.Length} 0"
            keyword.Substring(0, index) + "x" + keyword.Substring(index)
        | 1 ->
            printfn $"{keyword} {keyword.Length} 1"
            keyword.Substring(0, index) + " " + keyword.Substring(index, keyword.Length - index)
        | _ -> failwith "Invalid random number"

    let private mkError (program: string) (e: SyntaxError) =
        let lines = program.Split "\n"

        let mkErrorByPattern (patterns: string array) (breaker: string -> string) =
            let pattern = patterns.[rnd.Next(0, patterns.Length)]
            let regex = Regex pattern
            regex.Replace(program, breaker pattern, 1)

        let mkErrorByRemove patterns = mkErrorByPattern patterns (fun _ -> "")

        match e with
        | KeywordError -> mkErrorByPattern keywords breakKeyword
        | BracketError -> mkErrorByRemove brackets
        | OperationError -> mkErrorByRemove operations
        | DotCommaError -> mkErrorByRemove [| ";" |]
        | TruncatedBlockError ->
            let indexFrom = rnd.Next(0, lines.Length)
            let indexTo = rnd.Next(indexFrom, lines.Length)
            let newLines = Array.concat [ lines[0..indexFrom]; lines[indexTo..] ]
            String.concat "\n" newLines
        | TruncatedLineError ->
            let index = rnd.Next(0, lines.Length)
            let line = lines.[index]
            let truncateIndex = rnd.Next(0, line.Length)
            lines[index] <- line.Substring(0, truncateIndex)
            String.concat "\n" lines



    let private samples = [| Tests.GolangRSM.functionSample; Tests.GolangRSM.cycleSample; Tests.GolangRSM.expressionSample |]

    let generateProgramWithError (size: int) (errorCnt: int) (allowedErrors: SyntaxError array) =
        let mutable program = ""
        while program.Length < size do
            program <- program + samples.[rnd.Next(0, samples.Length)]

        let mutable errors = []
        for i in 1..errorCnt do
            let errorIndex = rnd.Next(0, allowedErrors.Length)
            let error = allowedErrors[errorIndex]
            errors <- (SyntaxError.toInt error).ToString() :: errors
            program <- mkError program error

        while program.Length < size do
            program <- program + samples.[rnd.Next(0, samples.Length)]

        program, errors


let private saveProgram (size: int) (errors: string list) (path: string) (program: string)  =
    let fileName = $"""test_{size.ToString()}_{String.concat "" errors}"""
    let filePath = $"""{path}{Path.DirectorySeparatorChar}{fileName}"""
    File.WriteAllText (filePath, program)

let private generateBenchmarkDataByConfig (config: DataGeneration.Config)=
    for size in config.sizes do
        for errorCnt in config.errorsCount do
            let program, errors = DataGeneration.generateProgramWithError size errorCnt config.allowedErrors
            saveProgram size errors workDirectory program

type BenchmarkData = {
    Name: string
    Text: string
    RSM: unit -> RSM.RSM
    StartVertex: LinearInputGraphVertexBase
    FinishVertex: LinearInputGraphVertexBase
    Size: int
    Weight: int
    Errors: string list
    DescriptorsCount: int
    ErrorsCount: int
} with override x.ToString() = x.Name

let mutable (benchmarkData: BenchmarkData array) = [| |]

let private loadBenchmarkDataText () =
    Directory.GetFiles(workDirectory)
    |> Array.filter (fun x ->  x.Contains "test")
    |> Array.map (fun x -> Path.GetFileName x, File.ReadAllText x)

let private getDataFromText (name: string, text: string) =
    let graph = text |> Tests.LinearGraphReader.mkLinearGraph id
    let startV = 0
    let finalV = graph.NumberOfVertices() - 1

    let startVertex,mapping = graph.ToCfpqCoreGraph startV
    let finalVertex = mapping[finalV]
    let rsm = Tests.GolangRSM.golangRSM ()
    let result, descriptorsCount = GLL.errorRecoveringEval finalVertex startVertex rsm GLL.AllPaths

    let weight =
        match result with
        | GLL.QueryResult.MatchedRanges _ ->

            let sppf = rsm.OriginalStartState.NonTerminalNodes.ToArray()
            let root = sppf |> Array.filter (fun n -> startVertex = n.LeftPosition && finalVertex = n.RightPosition) |> Array.minBy(fun n -> n.Weight)
            root.Weight |> int
        | _ -> failwith "Result should be MatchedRanges"

    // Expected format: test_{size}_{errors list}
    // Example: test_1000_123 -> (1000, ["KeywordError"; "BracketError"; "OperationError" ])
    let splitData (filename: string) =
        let parts = filename.Split("_")
        assert (Array.length parts = 3)
        let size = int parts[1]
        let errors =
            parts[2]
            |> seq
            |> List.ofSeq
            |> List.map (string << DataGeneration.SyntaxError.fromInt << int << string)
        size, errors

    let (size, errors) = splitData name

    {
        Name = name
        Text = text
        Size = size
        RSM = Tests.GolangRSM.golangRSM
        StartVertex = startVertex
        FinishVertex = finalVertex
        Weight = weight
        Errors = errors
        ErrorsCount = List.length errors
        DescriptorsCount = descriptorsCount
    }

let private refreshData (data: BenchmarkData) =
     let graph = data.Text |> Tests.LinearGraphReader.mkLinearGraph id
     let startV = 0
     let finalV = graph.NumberOfVertices() - 1

     let startVertex,mapping = graph.ToCfpqCoreGraph startV
     let finalVertex = mapping[finalV]

     {
        Name = data.Name
        Text = data.Text
        Size = data.Size
        RSM = Tests.GolangRSM.golangRSM
        StartVertex = startVertex
        FinishVertex = finalVertex
        Weight = data.Weight
        Errors = data.Errors
        ErrorsCount = data.ErrorsCount
        DescriptorsCount = data.DescriptorsCount
    }

let private loadBenchmarkData () =
    benchmarkData <- loadBenchmarkDataText () |> Array.map getDataFromText

let generateBenchmarkData () =
    let config = DataGeneration.Config.parse (System.IO.File.ReadAllText configPath)
    generateBenchmarkDataByConfig config

let reloadBenchmarkData () =
    match benchmarkData with
    | [||] -> benchmarkData <- loadBenchmarkDataText () |> Array.map getDataFromText
    | _ -> ()
    benchmarkData <- benchmarkData |> Array.map refreshData
    benchmarkData
