module myfableapp

open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Elmish
open Elmish.React

open Domain.Domain
open Parser

type Page = Entry | Stats

type Model = { ActivePage : Page; CurrentBall : Ball; Set : Set; Match : Match }

type Messages = | SwitchPage of Page
                | GameEntry of Event

let init () = { ActivePage = Stats; CurrentBall = Ball []; Set = Set []; Match = { Sets = []; Red = SingleTeam "Player 1"; Blue = SingleTeam "Player 2"}  }

let update msg model = 
    match msg with
    | SwitchPage p -> { model with ActivePage = p }
    | GameEntry e ->
        match e with
        | Goal c ->
            let finishedBall = model.CurrentBall |> Ball.addEvent e
            let updatedSet = model.Set |> Set.addBall finishedBall
            let startPos = Possession (otherColor c, Midfield)
            { model with Set = updatedSet; CurrentBall = Ball [ startPos ]; Match = { model.Match with Sets = [ updatedSet ] } }
        | _ -> { model with CurrentBall = model.CurrentBall |> Ball.addEvent e }

let classes xs = xs |> List.map (fun x -> x,true) |> classList

let makeVertical elems = div [ classes [ "tile"; "is-vertical" ] ] elems

let makeBox (stat : MatchSummary) =
    div [ classes [ "tile"; ]] [
        div [ classes [ "tile"; "is-4 "]] [] 
        div [ classes [ "box"; "tile"; "is-parent"; "is-vertical" ] ]
            [
                div [ classes [ "content"; "has-text-centered"; "tile"; "is-child"  ] ]
                    [ 
                        p [ classes [ "title" ]; ] [ unbox stat.StatName ]
                    ]

                div [ classes [ "level"; "is-child"; "tile" ] ]
                    [
                        div [ classes [ "level-left"; ] ]
                            [
                                p  [ classes [ "level-item"; "has-text-centered" ] ]
                                   [ unbox stat.RedTeam.name
                                     br []
                                     unbox <| stat.Red.MatchTotal.ToString()
                                   ]
                            ]
                        div [ classes [ "level-right"; ] ]
                            [
                                p  [ classes [ "level-item"; "has-text-centered" ] ]
                                   [ unbox stat.BlueTeam.name
                                     br []
                                     unbox <| stat.Blue.MatchTotal.ToString()
                                   ]
                            ]
                    ]
            ]
        div [ classes [ "tile"; "is-4 "]] [] 
    ]

let makeNav model dispatch =
    let tabClasses page =
        if (page = model.ActivePage) then
           classes [ "nav-item"; "is-tab"; "is-active" ]
        else
           classes [ "nav-item"; "is-tab" ]
        
    nav [ classes ["nav"] ]
        [
            div [ classes [ "nav-left"] ]
                [
                    a [ classes [ "title"; "nav-item" ] ]
                      [ unbox "FoosStat" ]
                    
                    a [ tabClasses Entry ; OnClick (fun _ -> dispatch (SwitchPage Entry)) ] 
                      [ unbox "Entry" ]
                    
                    a [ tabClasses Stats; OnClick (fun _ -> dispatch (SwitchPage Stats)) ]
                      [ unbox "Stats" ]
                ]
        ]

let makeStatsPage model =
    let elements = model.Match |> Domain.Analytics.matchSummary |> List.map makeBox
    div [ classes ["tile"; "is-ancenstor"] ] [ makeVertical elements ]

type EntryButtonType = | Goal of PlayerColor | Rod of (Rod * PlayerColor)
let makeButtons dispatch =
    let makeButton ebp =
        let color = match ebp with
                    | Goal Red
                    | Rod (_, Red) -> "is-danger"
                    | _ -> "is-primary"
        let text = match ebp with
                   | Goal c -> sprintf "Goal for %A" c
                   | Rod (Defence, c) -> sprintf "%A 2-Bar" c
                   | Rod (Midfield, c) -> sprintf "%A 5-Bar" c
                   | Rod (Attack, c) -> sprintf "%A 3-Bar" c

        let msg = match ebp with
                  | Rod (r, c) -> Possession (c, r)
                  | Goal c -> Event.Goal c
                  |> GameEntry

        div [ classes [ "tile" ] ]
            [
                div [ classes ["tile"; "is-5" ] ] []
                div [ classes ["tile"; "is-parent"; "is-2" ] ] [
                    a [ classes ["button"; "tile"; "is-child"; color; "is-large";  ]; OnClick (fun _ -> dispatch msg) ]
                      [
                          unbox text
                      ]
                ]
                div [ classes ["tile"; "is-5" ] ] []
            ]
    [ Goal Blue
      Rod (Defence, Red)
      Rod (Attack, Blue)
      Rod (Midfield, Red)
      Rod (Midfield, Blue)
      Rod (Attack, Red)
      Rod (Defence, Blue)
      Goal Red            ]
    |> List.map makeButton

let makeEntryPage model dispatch =
    let ballText =
        match model.CurrentBall with
        | Ball events -> events |> List.map (fun e -> e.ToString()) |> String.concat " - "

    let ballElem = p [ classes ["title"] ] [ unbox ballText ]

    let elementList = ballElem :: (makeButtons dispatch)
    div [ classes ["tile"; "is-ancenstor"] ] [ makeVertical elementList ]

let view model dispatch = 
    let page = match model.ActivePage with
               | Entry -> makeEntryPage model dispatch
               | Stats -> makeStatsPage model
    div []
        [
            makeNav model dispatch
            br []
            page
        ]

Program.mkSimple init update view
|> Program.withReact "contentDiv"
|> Program.run
