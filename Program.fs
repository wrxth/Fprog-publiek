/// Compositiewortel. De enige plek in het project die zowel de datalaag als de
/// servicelaag kent: hier wordt bepaald wélke implementatie achter elke poort
/// van de applicatielaag zit. Nergens anders is die keuze zichtbaar, en daarom
/// kan de opslag worden vervangen door alleen deze regels aan te passen.
module DrizzleCarton.Program

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open DrizzleCarton.Application

let configureApp (app: IApplicationBuilder) = app.UseGiraffe Service.Routes.webApp

let configureServices (services: IServiceCollection) =
    let db = Database.connect ()

    services.AddSingleton<IUserRepository>(Data.Users.UserRepository db :> IUserRepository)
    |> ignore

    services.AddSingleton<IEntryRepository>(Data.Files.EntryRepository db :> IEntryRepository)
    |> ignore

    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder.Configure(configureApp).ConfigureServices configureServices
            |> ignore)
        .Build()
        .Run()

    0
