module DrizzleCarton.Service.Routes

open Giraffe

let webApp: HttpHandler =
    choose
        [
          // Component Users
          GET >=> route "/user" >=> Users.all
          POST >=> route "/user" >=> Users.create
          GET >=> routef "/user/%i" Users.byId
          PUT >=> routef "/user/%i/username" Users.rename

          // Component Files
          GET >=> routef "/user/%i/file" Files.tree
          POST >=> routef "/user/%i/folder" Files.createFolder
          GET >=> routef "/user/%i/quota" Files.quota
          GET >=> routef "/user/%i/largest" Files.largest ]
