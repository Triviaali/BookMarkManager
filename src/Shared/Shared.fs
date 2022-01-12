namespace Shared

open System


type Bookmark = { Id: Guid
                  Title: string
                  Url: string
                  Tags: string list
                  DateCreated: DateTime
                  Read: bool
                  Favourite: bool
                  }

module Bookmark =
    let splitTags (tags: string) =
        let splitted = tags.Split(",") |> List.ofArray
        splitted

    let isValid (url: string) =
        String.IsNullOrWhiteSpace url |> not


    let create (title: string) (url: string) (tags:string) (read: bool) (favourite: bool) =
        {Id = Guid.NewGuid()
         Title = title
         Url = url
         Tags = splitTags tags
         DateCreated = DateTime.Now
         Read = read
         Favourite = favourite}

type IBookmarksApi =
    { getBookmarks: unit -> Async<Bookmark list>
      addBookmark: Bookmark -> Async<Bookmark> }


module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
