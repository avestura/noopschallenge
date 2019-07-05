// Noops Challange F# Solution
// You'll need Json Type Provider in FSharp.Data (https://fsharp.github.io/FSharp.Data/library/JsonProvider.html)

open System
open FSharp.Data
open System.Net

let initialPath = "https://api.noopschallenge.com"

[<Literal>]
let ResultSample = """
{
    "message":" ",
    "result":" ",
    "nextQuestion":" ",
    "numbers" : [1,2,3],
    "rules": [
        { "number":2, "response":"Beep" },
        { "number":5, "response":"Boop" }
    ]
}
"""
type FizzResponse = JsonProvider<ResultSample>

type GameRule = { num : int; resp : string }

type GameRules =  { rule1 : GameRule; rule2 : GameRule ; rule3 : GameRule option }

let defaultRules =
    { rule1 = {num = 3; resp = "Fizz" }
      rule2 = {num = 5; resp = "Buzz" }
      rule3 = None }

let getGameLevel innerUrl = sprintf "%s%s" initialPath innerUrl


let load (path:string) = FizzResponse.Load path

let parse (json:string) = FizzResponse.Parse json

let post url answer =
    Http.Request (
        getGameLevel url,
        httpMethod="POST",
        headers=["Content-Type", "application/json"],
        body = TextRequest (sprintf "{\"answer\":\"%s\"}" answer)
    )

let getStringOfBody respBody =
    match respBody with
    | Text s -> s
    | Binary _ -> ""

let allMods num (nums:int list) = nums |> List.forall (fun x -> num % x = 0)

let fizzBuzz num (r:GameRules) =
    let r1, r2 = r.rule1, r.rule2
    match (num , r.rule3) with
    | (a, Some r3) when allMods a [r1.num; r2.num; r3.num] -> sprintf "%s%s%s" r1.resp r2.resp r3.resp
    | (a, Some r3) when allMods a [r1.num; r3.num] -> sprintf "%s%s" r1.resp r3.resp
    | (a, Some r3) when allMods a [r2.num; r3.num] -> sprintf "%s%s" r2.resp r3.resp
    | (a, Some r3) when a % r3.num = 0 -> r3.resp
    | (num, _) ->
        match num with
        | a when allMods a [r1.num; r2.num] -> sprintf "%s%s" r1.resp r2.resp
        | a when a % r1.num = 0 -> r1.resp
        | a when a % r2.num = 0 -> r2.resp
        | a -> a.ToString()

let fizzBuzzSolver (nums:int []) r = [for i in nums -> fizzBuzz i r ] |> String.concat " "


let rec play path =
    let resp = load (getGameLevel path)
    printfn "%s" resp.Message
    if resp.NextQuestion = "" then

        let shouldMachineAnswer = Seq.isEmpty resp.Numbers |> not

        let sendAnswer answer =
            let rawResult = answer |> post path
            let resp2 = rawResult.Body |> getStringOfBody |> parse
            printfn "\n%s\n" resp2.Message
            if rawResult.StatusCode = 400 then
                play path
            else if (resp2.Result <> "interview complete") then
                play resp2.NextQuestion

        if shouldMachineAnswer then
            printfn "got: %A" resp.Numbers
            let rules =
                if resp.Rules.Length <> 0 then
                    { rule1 = { num = resp.Rules.[0].Number
                                resp = resp.Rules.[0].Response}
                      rule2 = { num = resp.Rules.[1].Number
                                resp = resp.Rules.[1].Response}
                      rule3 = if resp.Rules.Length >= 3 then
                                   Some ({ num = resp.Rules.[2].Number
                                           resp = resp.Rules.[2].Response})
                              else None }
                 else defaultRules

            let solve = fizzBuzzSolver resp.Numbers rules
            printfn "Solved: %s" solve
            solve |> sendAnswer
        else
            printf "Enter your answer: "
            Console.ReadLine() |> sendAnswer

    else
        play resp.NextQuestion

[<EntryPoint>]
let main argc =
    play "/fizzbot"

    0 // return 0 as exit result