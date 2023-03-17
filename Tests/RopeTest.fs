module Tests.RopeTest

open System
open Tests.ModelBased



[<AbstractClass>]
type RopeCommand<'c> () =
    inherit Command<char Rope.Rope, string, 'c, int>()

    let contentEq actual model =
        let content = String(Rope.getContent actual)
        if model = content then
            None
        else
            Some $"Actual <> Model\n\tActual: {content}\n\tModel: {model}"

    let correctHeight (actual: char Rope.Rope) _ =
        let rec calcHeight =
            function
            | Rope.Leaf _ -> 0
            | Rope.Node node -> 1 + max (calcHeight node.Left) (calcHeight node.Right)
        let realHeight = calcHeight actual
        if actual.Height = realHeight then
            None
        else
            Some $"Incorrect height {realHeight} <> {actual.Height}"

    override this.Validate actual model =
        List.fold (fun s f -> if s = None then f actual model else s) None [
            contentEq
            //correctHeight
        ]

    override this.GetGenerationInfo model = model.Length

type InsertCommand () =
    inherit RopeCommand<int * char array> ()
    let generateChar (rnd: Random) =
        let letters = "abcdefghijklmnopqrstuvwxyz"
        letters[rnd.Next(0, letters.Length)]

    override this.GenArgs len rnd =
        let index = rnd.Next(0, len)
        let size = rnd.Next(0, 5)
        index, Array.init size (fun _ -> generateChar rnd)

    override this.ModelOperation model args =
        let index, content = args
        model.Substring(0, index) + String(content) + model.Substring(index)

    override this.Operation actual args =
        let index, content = args
        Rope.insert actual index content

    override this.ToString args =
        let index, content = args
        $"insert {index} {String content}"

type DeleteCommand () =
    inherit RopeCommand<int * int> ()
    override this.GenArgs len rnd =
        let toIndex = rnd.Next(0, len)
        let fromIndex = rnd.Next(0, toIndex)
        fromIndex, toIndex

    override this.ModelOperation model args =
        let fromIndex, toIndex = args
        model.Substring(0, fromIndex) + model.Substring(toIndex)

    override this.Operation actual args =
        let fromIndex, toIndex = args
        Rope.delete actual fromIndex toIndex

    override this.ToString args =
        let fromIndex, toIndex = args
        $"delete {fromIndex} {toIndex}"

let specification seed =
    Specification<char Rope.Rope, string>(
        [|
            InsertCommand ()
            //DeleteCommand ()
        |],
        Rope.empty,
        "",
        seed
    )


