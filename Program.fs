module Validation

open System.Net.Mail

type Input =
    { Name: string option
      Age: int option
      Email: string option }

type ValidInput =
    { Name: string
      Age: int
      Email: string }

module Result =
    let merge x y =
        match x, y with
        | (Ok xres, Ok yres) -> Ok(xres, yres)
        | (Error(f, e1s), Error(g, e2s)) -> Error(f >> g, e2s @ e1s)
        | (Error e, Ok _) -> Error e
        | (Ok _, Error e) -> Error e

type ValidationBuilder() =
    member _.BindReturn(x, f) = Result.map f x
    member _.MergeSources(x, y) = Result.merge x y

[<AutoOpen>]
module ComputationExpressions =
    let validation = ValidationBuilder()

let isValidEmail email =
    try
        new MailAddress(email) |> ignore
        true
    with _ ->
        false

let validateName ({ Name = name }: Input) =
    match name with
    | Some n when n.Length > 3 -> Ok n
    | Some _ -> Error((fun (args: Input) -> { args with Name = None }), [ "must be at least 3 characters" ])
    | None -> Error(id, [ "name is required" ])

let validateAge ({ Age = age }: Input) =
    match age with
    | Some n when n > 18 -> Ok n
    | Some _ -> Error((fun (args: Input) -> { args with Age = None }), [ "must be at least 18 years old" ])
    | None -> Error(id, [ "age is required" ])

let validateEmail ({ Email = email }: Input) =
    match email with
    | Some e when isValidEmail e -> Ok e
    | Some _ -> Error((fun (args: Input) -> { args with Email = None }), [ "email address is invalid" ])
    | None -> Error(id, [ "email address is required" ])

let validateInput (args: Input) =
    validation {
        let! name = validateName args
        and! age = validateAge args
        and! email = validateEmail args

        return
            { Name = name
              Age = age
              Email = email }
    }
    |> Result.mapError (fun (f, messages) -> f args, messages)

let testData: Input =
    { Name = Some "bob"
      Age = Some 16
      Email = Some "Hello" }

let result = validateInput testData

printfn "Result: %A" result
// Result: Error
//   ({ Name = None
//      Age = None
//      Email = None },
//    ["email address is invalid"; "must be at least 18 years old";
//     "must be at least 3 characters"])
