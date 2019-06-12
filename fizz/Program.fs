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

type GameRule = { number : int; response : string }

type GameRules =  { rule1 : GameRule; rule2 : GameRule ; rule3 : GameRule option }

let defaultRules =
    { rule1 = {number = 3; response = "Fizz" }
      rule2 = {number = 5; response = "Buzz" }
      rule3 = None }

let getGameLevel innerUrl = sprintf "%s%s" initialPath innerUrl

type FizzResponse = JsonProvider<ResultSample>

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
    | (a, Some r3) when allMods a [r1.number; r2.number; r3.number] -> sprintf "%s%s%s" r1.response r2.response r3.response
    | (a, Some r3) when allMods a [r1.number; r3.number] -> sprintf "%s%s" r1.response r3.response
    | (a, Some r3) when allMods a [r2.number; r3.number] -> sprintf "%s%s" r2.response r3.response
    | (a, Some r3) when a % r3.number = 0 -> r3.response
    | (num, _) ->
        match num with
        | a when allMods a [r1.number; r2.number] -> sprintf "%s%s" r1.response r2.response
        | a when a % r1.number = 0 -> r1.response
        | a when a % r2.number = 0 -> r2.response
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
                    { rule1 = { number = resp.Rules.[0].Number
                                response = resp.Rules.[0].Response}
                      rule2 = { number = resp.Rules.[1].Number
                                response = resp.Rules.[1].Response}
                      rule3 = if resp.Rules.Length >= 3 then
                                   Some ({ number = resp.Rules.[2].Number
                                           response = resp.Rules.[2].Response})
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