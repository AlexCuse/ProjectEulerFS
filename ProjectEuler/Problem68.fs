module ProjectEuler.Problem68

#light

open BigIntUtil

type Segment (f, s, t) =
    member x.First = f
    member x.Second = s //here will be intersection with previous segment
    member x.Third = t  //here will be intersection with next segment

    member x.Sum = x.First + x.Second + x.Third

    member x.Digits = 
        seq {
                yield x.First
                yield x.Second
                yield x.Third
        } |> List.ofSeq        

    member x.ContainsDigit(d) =
        x.First = d || x.Second = d || x.Third = d

    override x.ToString() =
        x.Digits |> List.fold (fun acc x -> acc + x.ToString()) ""

    override x.Equals(that) =
        match that with
        | :? Segment as seg -> seg.First = x.First && seg.Second = x.Second && seg.Third = x.Third
        | _ -> false

let potentialSegments =
    let digits = [1I..10I]
    seq {
        for n in digits do
            for n' in digits |> List.filter (fun x -> not (x = n)) do
                for n'' in digits |> List.filter (fun x -> not (x = n || x = n')) do
                    yield Segment(n, n', n'')
    } |> List.ofSeq

let findNextSegmentCandidates (candidates:List<Segment>) (current:Segment) (segmentsSoFar:List<Segment>) isLast =
    let firstSecond = segmentsSoFar |> List.head |> fun x -> x.Second
    let excludeDigits =
        segmentsSoFar 
            |> List.map (fun s -> s.Digits) 
            |> List.collect (fun l -> l) 
            |> List.filter (fun d -> (not(isLast) || not (d = firstSecond)) && not (d = current.Third))
            |> Set.ofList   

    candidates
        |> List.filter (fun s -> not (segmentsSoFar |> List.exists (fun x -> x = s)))
        |> List.filter (fun s -> not (excludeDigits |> Set.contains s.First))
        |> List.filter (fun s -> (isLast && s.Third = firstSecond) || (not (isLast) && not (excludeDigits |> Set.contains s.Third)))
        |> List.filter (fun s -> s.Second = current.Third)

let rings (s:Segment) =
    let allCandidates = potentialSegments |> List.filter (fun x -> x.Sum = s.Sum && not (s = x))

    seq {
        for s1 in findNextSegmentCandidates allCandidates s [s] false do
            for s2 in findNextSegmentCandidates allCandidates s1 [s;s1] false do
                for s3 in findNextSegmentCandidates allCandidates s2 [s;s1;s2] false do
                    for s4 in findNextSegmentCandidates allCandidates s3 [s;s1;s2;s3] true do
                        yield [s;s1;s2;s3;s4]
    } |> List.ofSeq

let score (x:List<Segment>) =
    let pivot = x |> List.findIndex (fun v ->
        v.First = (x |> List.map (fun x -> x.First) |> List.min))

    let x' = x |> Array.ofList
    let p1 = x'.[pivot..4] |> List.ofArray
    let p2 = 
        if pivot = 0 then [] else x'.[0..(pivot-1)] |> List.ofArray

    x, (p1 @ p2) 
        |> List.map (fun seg -> seg.Digits) 
        |> List.collect (fun dgts -> dgts)
        |> List.map (fun dgt -> digitsFrom dgt) //handle splitting ten
        |> List.collect (fun dgts -> dgts)
        |> toNumber

let solve =
    //10 must appear in first position (meaning it only shows once, otherwise there would be 17 digits - so 1 occurrence of the "winning" pattern has got to have 10 in the first position of first segment)
    potentialSegments
          |> Seq.filter (fun s -> s.First = 10I) 
          |> Seq.map rings
          |> Seq.collect (fun l -> l)
          |> Seq.distinct
          |> Seq.map score
          |> Seq.maxBy(fun (_, scr) -> scr)
    



