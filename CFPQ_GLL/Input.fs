module CFPQ_GLL.Input

type Range= {
    fromIndex: int
    toIndex: int
}

type IDynamicParser =
    abstract member Replace: Range -> Range -> string -> unit
    abstract member CurrentParseTree: SPPF.MatchedRanges with get
    abstract member CurrentInput: string with get

//
