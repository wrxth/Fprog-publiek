module DrizzleCarton.Model.Users

open DrizzleCarton.Model.Ids

// Username


type Username = private Username of string

module Username =
    let private reserved = [ "drizzle"; "carton" ]

    let create (s: string) : Result<Username, string> =
        let lowered = s.ToLowerInvariant()

        if s.Contains " " then
            Error "Username may not contain spaces"
        elif reserved |> List.exists lowered.Contains then
            Error "Username may not mention 'drizzle' or 'carton'"
        else
            Ok(Username s)

    let value (Username s) : string = s

// Quota

type Quota = private Quota of int

module Quota =

    let private minBytes = 1024 * 1024
    let private maxBytes = 1024 * 1024 * 1024

    let create (n: int) : Result<Quota, string> =
        if n >= minBytes && n <= maxBytes then
            Ok(Quota n)
        else
            Error $"Quota must be at least {minBytes} and at most {maxBytes} bytes, got {n}"

    let value (Quota n) : int = n

// User


type User =
    { Id: UserId
      Username: Username
      Quota: Quota
      RootFolder: EntryId }

module User =

    let rename (name: Username) (user: User) : User = { user with Username = name }
