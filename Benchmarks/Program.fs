open BenchmarkDotNet.Running
open Benchmarks
open Benchmarks.Benchmark

[<EntryPoint>]
let main args =
    // Все еще нужно хардкодить путь в DataGeneration (из-за ограничений BenchmarkDotNet)
    // Конфиг для генерации данных читается из той же папки
    match args[0] with
    | "--run" ->
        let summary = BenchmarkRunner.Run<ErrorRecoveringBenchmark>()
        printfn $"Result directory: {summary.ResultsDirectoryPath}"
    | "--generate" ->
        for file in System.IO.Directory.GetFiles(BenchmarkData.workDirectory)
            do if file.Contains "test" then System.IO.File.Delete file
        BenchmarkData.generateBenchmarkData ()
    | "--debug" ->
        printfn "debug"
    | _ -> failwith "Unexpected argument"
    0
