module Tests.ModelBased

open System
open System.Text

type ICommand<'a, 'b> =
    abstract member Step: 'a -> 'b -> Random -> (string -> unit) -> Result<'a * 'b, string>

[<AbstractClass>]
type Command<'a, 'b, 'c, 'd> () =
    // 'a - testing type
    // 'b - model type
    // 'c - command args type
    // 'd - gen info type
    abstract member Operation: 'a -> 'c -> 'a
    abstract member ModelOperation: 'b -> 'c -> 'b
    abstract member GenArgs: 'd -> Random -> 'c
    abstract member Validate: 'a -> 'b -> string option
    abstract member ToString: 'c -> string
    abstract member GetGenerationInfo: 'b -> 'd

    
    interface ICommand<'a, 'b> with
        override this.Step actual model rnd log =
            let info = this.GetGenerationInfo model
            let args = this.GenArgs info rnd 
            let newActual = this.Operation actual args
            let newModel = this.ModelOperation model args
            log (this.ToString args)
            match this.Validate newActual newModel with
            | Some err -> Error err
            | None -> Ok (newActual, newModel)

type Specification<'a, 'b> (
        commands: ICommand<'a, 'b> array,
        initialActual: 'a,
        initialModel: 'b,
        rndSeed: int
    ) =

    let rnd = Random(rndSeed)
    let mutable currentActual = initialActual
    let mutable currentModel = initialModel
    let appliedCommands = System.Collections.Generic.List<string>()

    member private this.Step() =
        let command = commands[rnd.Next(0, commands.Length)]
        let log = appliedCommands.Add 
        command.Step currentActual currentModel rnd log

    member this.Verify (stepsCount: int) =
        if stepsCount = 0 then printfn "Model checking successfully finished!"; 0
        else
            match this.Step () with
            | Error err ->
                let output = StringBuilder()
                output.Append $"Model checking failed with error:\n{err}\n" |> ignore
                output.Append "After operations:\n" |> ignore
                for c in appliedCommands do
                    output.Append $"\t{c}\n" |> ignore
                printfn $"{output.ToString()}"; 1
            | Ok (actual, model) ->
                currentActual <- actual
                currentModel <- model
                this.Verify (stepsCount - 1)

    member this.RunWithTimeout (stepsCount: int) (timeLimit: int) =
        try
            let task = task { return this.Verify stepsCount }
            if task.Wait timeLimit then task.Result else printfn "Finished by timeout"; 0 
        with
            | :? AggregateException  -> reraise (); 1