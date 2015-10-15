namespace AboutMath

open System
open System.Numerics
open Microsoft.FSharp
open Microsoft.FSharp.Math
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

module AboutMath =
    let i = Complex.ImaginaryOne
    let _0 = Complex.Zero
    let c x = new Complex(float(x), 0.0)

    let inline isAlternative (m : Matrix<'T>) =
        m |> Matrix.foralli (fun i j x -> i < j || x = - m.[j, i]) 

    let sample() =
        let b1 = DenseMatrix.ofRowList( [   [_0; i * (c -2); _0]
                                            [(c 2) * i; _0; _0]
                                            [_0; _0; _0] ])
        let b2 = DenseMatrix.ofRowList( [   [_0; _0; c -1]
                                            [_0; _0; -i]
                                            [c 1; i; _0] ])
        let b3 = DenseMatrix.ofRowList( [   [_0; _0; c 1]
                                            [_0; _0; -i]
                                            [c -1; i; _0] ])

        let bs = [b1; b2; b3]
        for b in bs do
            printfn "%s" (b.ToMatrixString())
            printfn "is alternative? : %s" ((isAlternative b).ToString())

        for k in 0..2 do
            for l in 0..2 do
                printfn "b%d . b%d:\r\n%s" k l ((bs.[k] * bs.[l]).ToMatrixString())

        let a1 = [3; 7; 2]
        let a2 = [5; 2; 3]
        let f = function
            | [x11; x12; x21] -> b1.Multiply(c x11) + b2.Multiply(c x12) + b3.Multiply(c x21)
            | _ -> failwith ""
        
        let inline bracket x y =
            x * y - y * x

        //(*
        printfn "[f X, f Y]"
        printfn "%s" ((bracket (f a1) (f a2)).ToString())

        let x1 = DenseMatrix.ofRowList [[c 3; c 7]; [c 2; c -3]]
        let y1 = DenseMatrix.ofRowList [[c 5; c 2]; [c 3; c -5]]
        printfn "x1 y1, y1 x1, [x1, y1]"
        printfn "%s" ((x1 * y1).ToString())
        printfn "%s" ((y1 * x1).ToString())
        printfn "%s" ((bracket x1 y1).ToString())
        //*)

        ()

