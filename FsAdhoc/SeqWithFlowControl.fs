namespace Uedai.Utilities

open System

    (*
module Builder =
    type BuilderWithFlowControl<'Bd, 'T, 'U
            when 'Bd : (member Zero : unit -> 'U)
            and  'Bd : (member Yield : 'T -> 'U)
            and  'Bd : (member YieldFrom: 'U -> 'U)
        > (bd : 'Bd) =
        member this.Zero() : 'U * FlowControl = (bd.Zero(), Break)

        member this.Yield(x) = (bd.Yield(x), Continue)
        member this.YieldFrom(xs) = (bd.YieldFrom(xs), Continue)
        member this.Return(x) = (bd.Return(x), Break)
        member this.ReturnFrom(xs) = (bd.ReturnFrom(xs), Break)

        member this.Delay f = bd.Delay(f)
        member this.Run   f = bd.Run(f() |> fst)

        member this.Bind(x, f) = (bd.Bind(xs, f), Continue)
        member this.Using(x, f) = bd.Using(x, f)
        member this.Combine((xs, flow), cont) =
            match flow with
            | Break -> (xs, Break)
            | Continue -> (bd.Combine(xs, cont), Continue)

        member this.TryWith(f, h) = bd.TryWith(f, handler)
        member this.TryFinally(f, c) = bd.TryFinally(f, c)
        member this.While(pred, f) = bd.While(pred, f)
        member this.For(src) = bd.For(src, f)
    //*)

    (*
    type MySeqBuilder internal () =
        member this.Zero() = Seq.empty
        member this.Delay f = f
        member this.Run f = f()

        member this.Yield(x) = Seq.singleton x
        member this.YieldFrom(xs) = xs
        member this.Return(x) = Seq.singleton x
        member this.ReturnFrom(xs) = xs

        member this.Bind(x, f) = Seq.collect (f >> fst) x
        member this.Using(x : #IDisposable, f) =
            try f x
            finally
                if x <> null then x.Dispose()

        member this.Combine(xs, cont) = Seq.append xs (cont())
        member this.TryWith(f, handler) =
            try f()
            with e -> handler e
        member this.TryFinally(f, compensation) =
            try f()
            finally compensation()
        member this.While(pred, f) =
            if pred() then
                this.Combine(f(), fun () -> this.While(pred, f))
            else this.Zero()
        member this.For(src : #seq<_>, f) =
            this.Using(
                src.GetEnumerator(),
                (fun iter -> this.While(iter.MoveNext, (fun () -> f iter.Current)))
            )

    let myseq = MySeqBuilder()
    let myseqBreakable<'T> = new BuilderWithFlowControl<MySeqBuilder, 'T, seq<'T>>()
    myseqBreakable {
        for i in 0..5 do
            if i < 2 then
                yield i
            else
                return i
    }
    |> printfn "%A"
    //*)

