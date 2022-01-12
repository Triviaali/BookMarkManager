module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type BookmarkPersistence() =
    let bookmarks = ResizeArray<_>()

    member __.GetBookmarks() = List.ofSeq bookmarks

    member __.AddBookmark(bookmark: Bookmark) =
        if Bookmark.isValid bookmark.Url then
            bookmarks.Add(bookmark)
            Ok()
        else
            Error "Url cant be blank"

let bookmarkPersistence = BookmarkPersistence()

bookmarkPersistence.AddBookmark(Bookmark.create "Metosin" "https://www.metosin.fi/en/" "Metosin,Clojure,Functional,Innovative" true true)
|> ignore

bookmarkPersistence.AddBookmark(Bookmark.create "On superorganisms - Metosin" "https://www.metosin.fi/blog/superorganism/" "Metosin,Superpowers" true true)
|> ignore

bookmarkPersistence.AddBookmark(Bookmark.create "Clojure" "https://clojure.org/" "Clojure, Functional" true true)
|> ignore

bookmarkPersistence.AddBookmark(Bookmark.create "History of Clojure" "https://clojure.org/about/history" "Clojure,History,Pdf" true true)
|> ignore

bookmarkPersistence.AddBookmark(Bookmark.create "History of Clojure Discussion" "https://news.ycombinator.com/item?id=23418699" "Clojure,History,Hackernews" false false)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Clojure, the Good Parts" "https://rasterize.io/blog/clojure-the-good-parts.html" "Clojure,Blog" false false)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Kari Marttila Blog" "https://www.karimarttila.fi/" "Metosin,Clojure,Cloud,AWS" true true)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Clojureverse" "https://clojureverse.org/" "Clojure,Discussion" false false)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Fable - Javascript you can be proud of" "https://fable.io/" "F#,Compiler,Fullstack" false false)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Elmish" "https://github.com/elmish/elmish" "F#,Elm,Github" false false)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Careers as fullstack Clojure Developer" "https://www.metosin.fi/en/careers/" "Metosin,Clojure,Fullstack,Interesting" true true)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Metosin Github" "https://github.com/metosin" "Metosin,Clojure,Github" true true)
|> ignore
bookmarkPersistence.AddBookmark(Bookmark.create "Reagent" "https://github.com/reagent-project/reagent" "Clojurescript,Clojure,React,Github" false true)
|> ignore



let bookmarksApi =
    { getBookmarks = fun () -> async { return bookmarkPersistence.GetBookmarks() }
      addBookmark =
          fun bookmark ->
              async {
                  match bookmarkPersistence.AddBookmark bookmark with
                  | Ok () -> return bookmark
                  | Error e -> return failwith e
              }}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue bookmarksApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
