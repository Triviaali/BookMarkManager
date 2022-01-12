module Index

open Elmish
open Fable.Remoting.Client
open Feliz.Bulma
open Feliz.style
open Shared
open System

type MainFilterType =
  | All
  | Starred
  | Unread
  | Read
  

type State = {Bookmarks: Bookmark list; Title: string; Url: string; Tags: string; MainFilter: MainFilterType; AllTags: Set<string>; ActivatedTags: Set<string>}


let isValid (stringi: string) =
    String.IsNullOrWhiteSpace stringi

type BMsg =
    | GotBookmarks of Bookmark list
    | SetTitle of string
    | SetUrl of string
    | SetTags of string
    | AddBookMark
    | AddedBookMark of Bookmark
    | MarkAsRead of Guid
    | MarkAsFavourite of Guid
    | DeleteBookmark of Guid
    | SetMainFilter of MainFilterType
    | ActivateTag of string

let bookmarkApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookmarksApi>

let init () : State * Cmd<BMsg> =
    let state = { Bookmarks = []; Title = ""; Url = ""; Tags = ""; MainFilter = All; AllTags = Set.empty<string>; ActivatedTags = Set.empty<string>}
    let cmd = Cmd.OfAsync.perform bookmarkApi.getBookmarks () GotBookmarks
    state, cmd


let update (msg: BMsg) (state: State) : State * Cmd<BMsg> =
    match msg with
    | GotBookmarks bookmarks -> { state with Bookmarks = bookmarks }, Cmd.none
    | SetTitle title -> { state with Title = title }, Cmd.none
    | SetUrl field -> { state with Url = field }, Cmd.none
    | SetTags field -> { state with Tags = field }, Cmd.none
    | AddBookMark ->
        let bookmark = Bookmark.create state.Title state.Url state.Tags false false
        let cmd = Cmd.OfAsync.perform bookmarkApi.addBookmark bookmark AddedBookMark

        { state with Title = ""; Url = "";  Tags=""}, cmd
    | AddedBookMark bm ->
        { state with Bookmarks = [ bm ] @ state.Bookmarks ; Title = ""; Url = "";  Tags="" }, Cmd.none
    | MarkAsFavourite bid ->
      let updatedBookmarks =
        state.Bookmarks
        |> List.map (fun bm ->
          if bm.Id = bid then {bm with Favourite = not bm.Favourite} else bm)
        
      {state with Bookmarks = updatedBookmarks }, Cmd.none
    | MarkAsRead bid ->
      let updatedBookmarks =
        state.Bookmarks
        |> List.map (fun bm ->
          if bm.Id = bid then {bm with Read = not bm.Read} else bm)
        
      {state with Bookmarks = updatedBookmarks }, Cmd.none
      
    | DeleteBookmark bid ->
      let updatedBookmarks =
        state.Bookmarks
        |> List.filter (fun bm -> bm.Id <> bid)
        
      {state with Bookmarks = updatedBookmarks}, Cmd.none
      
    | SetMainFilter (t: MainFilterType) ->
      {state with MainFilter = t}, Cmd.none
      
    |  ActivateTag (tag: string) ->
      let tagIsActivated = Set.contains tag state.ActivatedTags
      match tagIsActivated with
      | false -> {state with ActivatedTags = (Set.add tag state.ActivatedTags) }, Cmd.none
      | true -> {state with ActivatedTags = (Set.remove tag state.ActivatedTags) }, Cmd.none 
        

open Feliz

let addBookmarkForm (state: State) (dispatch: BMsg -> unit) =
    Bulma.box [
        prop.style [ style.padding 15 ]
        prop.children [
        Html.div [
            prop.children [
            Bulma.label "Title"
            Html.div [
                prop.classes ["control"]
                prop.children [
                    Html.input [
                        prop.classes ["input"; if isValid state.Title then "is-danger" ]
                        prop.type'.text
                        prop.placeholder "Title of site"
                        prop.valueOrDefault state.Title
                        prop.onChange (SetTitle >> dispatch)
                    ]
                ]
            ]
            ]
        ]
        Html.div [
            prop.children [
            Bulma.label "Url"
            Html.div [
                prop.classes ["control"]
                prop.children [
                    Html.input [
                        prop.classes ["input";  if isValid state.Url then "is-danger"]
                        prop.type'.text
                        prop.placeholder "Url of site"
                        prop.valueOrDefault state.Url
                        prop.onChange (SetUrl >> dispatch)
                    ]
                ]
            ]
            ]
        ]
        Html.div [
            prop.children [
            Bulma.label "Tags"
            Html.div [
                prop.className "control"
                prop.children [
                    Html.input [
                        prop.classes  ["input"; if isValid state.Tags then "is-danger"]
                        prop.type'.text
                        prop.placeholder "Tags separated by comma"
                        prop.valueOrDefault state.Tags
                        prop.onChange (SetTags >> dispatch)
                    ]
                ]
            ]
            ]
        ]
        Html.div [
            prop.style [style.marginTop 20;]
            prop.className "field"
            prop.children [
                Html.button [
                    prop.style [style.marginRight 20]
                    prop.classes ["button is-black"]
                    prop.onClick (fun _ -> dispatch AddBookMark)
                    prop.text "Add" ]

                ]]]]

let mainBookmarkFilter (bookmarks: Bookmark List) (mft: MainFilterType) =
  match mft with
  | All -> bookmarks
  | Starred -> bookmarks |> List.filter (fun bm -> bm.Favourite)
  | Unread -> bookmarks |> List.filter (fun bm -> bm.Read |> not)
  | Read -> bookmarks |> List.filter (fun bm -> bm.Read)
  
  
let anyBookMarkTagsActivated (bookmark: Bookmark) (activatedTags: Set<string>) : bool =
  let tags = bookmark.Tags
  tags |> List.map (fun tag -> Set.contains tag activatedTags) |> List.contains true
  
let filterBookMarks (state: State) =
  let firstRound = mainBookmarkFilter state.Bookmarks state.MainFilter
  let activatedTags = state.ActivatedTags |> List.ofSeq
  match activatedTags with
  | [] -> firstRound
  | _ -> firstRound |> List.filter (fun bm -> anyBookMarkTagsActivated bm state.ActivatedTags)

let renderTagFiltering (state: State) (dispatch: BMsg -> unit) =
  let concat_tags = state.Bookmarks |> List.map (fun bm -> bm.Tags) |> List.concat |> List.distinct
  let rows_of_four = concat_tags |> List.splitInto 4
  
  Html.div [
    prop.classes ["box"]
    prop.children [
      Html.div [
        prop.classes ["tile"; "is-ancestor"]
        prop.children [
          for row in rows_of_four do 
            Html.div [
              prop.classes ["tile"; "is-parent"; "is-vertical"]
              prop.children [
                for tag in row do
                Html.button [
                  prop.classes ["button"; "tile"; "is-child"; if Set.contains tag state.ActivatedTags then "is-primary"]
                  prop.text tag
                  prop.onClick (fun _ -> dispatch (ActivateTag tag))
                ] 
              ]
            ]
        ]
      ]
    ]
  ]
    

let renderFilterTabs (state: State) (dispatch: BMsg -> unit) =
  Html.div [
    prop.classes ["box"; "container"]
    prop.children [
      Html.div [
        prop.classes [ "buttons"; "is-toggled"; "is-full-width"; "tile"; "is-parent"]
        prop.children [
          Html.button [
            prop.classes ["button"; "tile"; if (state.MainFilter = All) then "is-success"; "is-active"]
            prop.text "All"
            prop.onClick (fun _ -> dispatch (SetMainFilter All))
          ]
          Html.button [
            prop.classes ["button"; "tile"; if (state.MainFilter = Starred) then "is-success"; "is-active"]
            prop.text "Starred"
            prop.onClick (fun _ -> dispatch (SetMainFilter Starred))
          ]
          Html.button [
            prop.classes ["button"; "tile"; if (state.MainFilter = Unread) then "is-success"; "is-active"]
            prop.text "Unread"
            prop.onClick (fun _ -> dispatch (SetMainFilter Unread))
          ]
          Html.button [
            prop.classes ["button"; "tile";  if (state.MainFilter = Read) then "is-success"; "is-active"]
            prop.text "Read"
            prop.onClick (fun _ -> dispatch (SetMainFilter Read))
          ]
        ]
      ]
    ]
  ]


let renderBookmarkCard (bookmark: Bookmark) (state: State) (dispatch: BMsg -> unit) =
  Bulma.card [
    prop.style [style.margin 10]
    prop.children [
      Bulma.cardHeader [
        prop.children [
        Html.p [
          prop.className "card-header-title"
          prop.text bookmark.Title
        ]
        if bookmark.Favourite then 
          (Html.button [
            prop.style [style.margin 5]
            prop.classes ["card-header-icon"]
            prop.children [
              Html.span [
                prop.classes ["icon"]
                prop.children [
                  Html.i [
                    prop.style [style.color.yellow]
                    prop.classes ["fas"; "fa-star"] 
                  ]
                ]
              ]
            ]
          ])
        Html.button [
          prop.style [style.margin 5]
          prop.classes ["card-header-icon"]
          prop.children [
            Html.span [
              prop.classes ["icon"]
              prop.children [
                Html.i [
                  prop.classes ["fas"; if bookmark.Read then "fa-box" else "fas fa-box-open"] // 
                ]
              ]
            ]
          ]
        ]
      ]]
      Bulma.cardContent [
        Bulma.content [
          Bulma.columns [
            prop.children [
              Html.div [
                prop.classes ["column"; "is-three-quarters"]
                prop.children [
                  Html.a [
                    prop.style [style.color.blue]
                    prop.text bookmark.Url
                    prop.href bookmark.Url
                  ]
                ]]
              Html.div [
                prop.classes ["column"]
                prop.text (bookmark.DateCreated.ToString "yyyy/MM/dd")
              ]]
            
          ]
        ]
      ]
      if (List.length bookmark.Tags > 0) then 
      (Bulma.cardContent [
        Html.div [
          prop.classes ["tile is-ancestor"]
          prop.children [
            let rows_of_four = bookmark.Tags |> List.splitInto 4
            for row in rows_of_four do
            Html.div [
              prop.classes ["tile"; "is-parent"; "is-vertical"]
              prop.children [
                for tag in row ->
                  Html.button [
                  prop.classes ["tile"; "is-child"; "button"; if Set.contains tag state.ActivatedTags then "is-primary"]
                  prop.text tag
                  prop.onClick (fun _ -> dispatch (ActivateTag tag))
                ]
              ]
            ]
          ]
        ]
      ])
      Bulma.cardFooter [
        prop.children [
          Html.a [prop.className "card-footer-item"
                  prop.text (if bookmark.Favourite then "Unstar" else "Star")
                  prop.onClick (fun _ -> dispatch (MarkAsFavourite bookmark.Id))]
          Html.a [prop.className "card-footer-item"
                  prop.text (if bookmark.Read then "Mark as unread" else "Mark as read")
                  prop.onClick (fun _ -> dispatch (MarkAsRead bookmark.Id))]
          Html.a [prop.className "card-footer-item"
                  prop.text "Delete"
                  prop.onClick (fun _ -> dispatch (DeleteBookmark bookmark.Id))]
          ]]
        ]
      ]
  
  
let renderBookMarkContainer (state: State) (dispatch: BMsg -> unit) =
  let filteredBookmarks = filterBookMarks state
  Html.div [
    Html.ul [
      for bookmark in filteredBookmarks -> renderBookmarkCard bookmark state dispatch
  ]
  ]


let view (state: State) (dispatch: BMsg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundColor color.beige
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroBody [
                //prop.className "is-align-items-flex-start"
                prop.style [alignItems.flexStart]
                prop.children [
                  Bulma.container [
                      Bulma.column [
                          column.is6Mobile
                          column.isOffset1
                          prop.children [
                              Bulma.title [
                                  text.hasTextCentered
                                  prop.text "BookMark Manager"
                                  prop.style [ style.color color.black ]
                              ]
                              addBookmarkForm state dispatch
                              renderFilterTabs state dispatch
                              renderTagFiltering state dispatch
                              renderBookMarkContainer state dispatch
                          ]
                      ]
                  ]
            ]
            ]
        ]
    ]
