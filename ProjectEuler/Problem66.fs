module ProjectEuler.Problem66

#light
 
let continuedFraction D =
    let getNextP a q p = a * q - p
    let getNextQ D pn q = (D - pown pn 2) / q
    let getNextA a0 pn qn = (a0 + pn) / qn
 
    let a0 = bigint(sqrt(double(D)))
    let p1, q1 = a0, D-a0*a0
    let a1 = getNextA a0 p1 q1

    let init = (p1, q1, a1)
 
    Seq.unfold (fun (p, q, a) ->
        let pn = getNextP a q p
        let qn = getNextQ D pn q
        let an = getNextA a0 pn qn
        Some((p, q, a), (pn, qn, an))) init
    |> Seq.map (fun (pn, qn, an) -> an)
    |> Seq.append [a0]

let continuedFractions D =
    let getValue an n' n'' = an * n' + n''
 
    let fractionTerms = continuedFraction D
    let a0 = Seq.head fractionTerms
    let p0, p1 = a0, a0 * (Seq.nth 1 fractionTerms) + 1I
    let q0, q1 = 1I, Seq.nth 1 fractionTerms
    let initial = (p1, q1, p0, q0)
 
    Seq.scan (fun (p, q, pn, qn) a ->
        let pn' = getValue a p pn
        let qn' = getValue a q qn
        (pn', qn', p, q)) initial (fractionTerms |> Seq.skip 2)
    |> Seq.map (fun (pn, qn, pn', qn') -> (pn, qn))
    |> Seq.append [(p0, q0)]

let maxValueForMinimalSolution D =
    let isDiophantine D x y = 
        x * x - D * y * y = 1I
    continuedFractions D
    |> Seq.filter (fun (x, y) -> isDiophantine D x y)
    |> Seq.map (fun (x, _) -> x)
    |> Seq.head

let solve =
    let isSquare D = 
        sqrt(double D) % 1.0 <> 0.0
    [1I..1000I]
    |> List.filter isSquare
    |> List.maxBy maxValueForMinimalSolution