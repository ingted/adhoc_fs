namespace AboutTyping

open System
open System.Timers
open System.Diagnostics
open Basis.Core
open Uedai.Utilities

module TypingSystem =
    /// タイピングの問題データ。今のところ1文のみ。
    type TypingQuestion (text : string) =
        let iter = text.GetEnumerator()
        let mutable finished
            = iter.MoveNext()

        member this.FullText = text
        member this.HasFinished = finished

        member this.Accept (c : char) =
            iter.Current = c
        member this.MoveNext() =
            iter.MoveNext()

    type TypingGame (question : TypingQuestion) =
        let mutable i = 0
        let mutable cntErrors = 0
        let stopwatch = new Stopwatch()

        member this.Begin() = stopwatch.Start()
        member this.End() = stopwatch.Stop(); true

        /// 入力
        member this.Input (c : char) =
            if question.Accept c then
                if question.MoveNext()
                    then this.End()
                    else false
            else false


