open Lwt.Infix

(** Utility module for local storage. *)
module Storage = struct
  open Js

  let storage =
    Optdef.case (Dom_html.window##localStorage)
      (fun () -> failwith "Storage is not supported by this browser")
      (fun v -> v)

  let key = string "jsoo-todo-state"

  let find () =
    let r = storage##getItem(key) in
    Opt.to_option @@ Opt.map r to_string

  let set v = storage##setItem(key, string v)

  let init default = match find () with
    | None -> set default ; default
    | Some v -> v
end

(** Application data *)
module Model = struct

  type visibility =
    Completed | Active | All
    deriving (Json)

  type task = {
    description : string ;
    (* backup field keep the previous description to restore it when ESC key is pressed *)
    backup : string ;
    completed : bool ;
    editing : bool ;
    id : int ;
  } deriving (Json)

  type t = {
    tasks : task list ;
    field : string ;
    uid : int ;
    visibility : visibility ;
  } deriving (Json) (* to save/restore the state in JSON *)

  let empty = {
    tasks = [] ;
    field = "" ;
    uid = 0 ;
    visibility = All ;
  }

  let new_task desc id = {
    description = desc ;
    backup = desc ;
    completed = false ;
    editing = false ;
    id = id
  }

  let string_of_visibility v =
    match v with
    | Completed -> "Completed"
    | Active -> "Active"
    | All -> "All"

  let from_json s =
    Json.from_string<t> s

  let to_json m =
    Json.to_string<t> m

end

(** User interface actions *)
module Action = struct

  type action =
    | Update_field of Js.js_string Js.t
    | Editing_task of (int * bool)
    | Update_task of (int * Js.js_string Js.t)
    | Add
    | Delete of int
    | Delete_complete
    | Check of (int * bool)
    | Check_all of bool
    | Change_visibility of Model.visibility
    | Escape of int

end

(** User actions are sent as events *)
let event_gui, send_event_gui = React.E.create ()

(** Signals to update view parts *)
let signal_entry, send_entry = React.S.create ""
let signal_list, send_list = React.S.create (Model.All, [])
let signal_controls, send_controls = React.S.create (Model.All, [])

(** Build HTML and send user actions *)
module View = struct

  open Action
  open Tyxml_js

  module Ev = Lwt_js_events
  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.(async @@ (fun () -> ev elem handler))

  let replace_children parent children =
    let rec remove () =
      let first = parent##firstChild in
        Js.Opt.case first
          (fun () -> ())
          (fun e -> Dom.removeChild parent e; remove ())
    in
    remove () ;
    Dom.appendChild parent children

  (* New task input field *)
  let task_entry =
    let task_input =
      Html5.(input ~a:[
          a_input_type `Text ;
          a_class ["new-todo"] ;
          a_placeholder "What needs to be done?" ;
          a_autofocus `Autofocus ;
          a_onkeypress (fun evt -> if evt##keyCode = 13 then send_event_gui Add; true) ;
        ] ())
    in
    let task_input_dom = To_dom.of_input task_input in
    let _ = React.S.map (fun v -> task_input_dom##value <- Js.string v) signal_entry in
    bind_event Ev.inputs task_input_dom (fun _ ->
      Lwt.return @@ send_event_gui (Update_field task_input_dom##value)) ;
    Html5.(header ~a:[a_class ["header"]] [
        h1 [ pcdata "todos" ];
        task_input
      ])

  (** One item in the tasks list *)
  let todo_item (todo:Model.task) =
    let input_check =
      Html5.(input ~a:(
          let l = [
            a_input_type `Checkbox ;
            a_class ["toggle"] ;
            a_onclick (fun _ ->
              send_event_gui (Check (todo.id, (not todo.completed))) ; true
            )]
          in if todo.completed then a_checked `Checked :: l else l
        ) ())
    in

    let input_edit =
      Html5.(input ~a:[
          a_input_type `Text ;
          a_class ["edit"] ;
          a_value todo.description ;
          a_id (Printf.sprintf "todo-%u" todo.id) ;
          a_onblur (fun _ ->
            send_event_gui (Editing_task (todo.Model.id, false)); true ) ;
        ] ())
    in
    let input_edit_dom = To_dom.of_input input_edit in

    bind_event Ev.inputs input_edit_dom (fun _ ->
      Lwt.return @@ send_event_gui (Update_task (todo.id, input_edit_dom##value))) ;

    let keypress_handler evt =
      if evt##keyCode = 13 then
        send_event_gui (Editing_task (todo.id, false))
      else if evt##keyCode = 27 then
        send_event_gui (Action.Escape todo.id)
      else () ;
      Lwt.return_unit
    in
    bind_event Ev.keypresses input_edit_dom keypress_handler ;

    let css_class l =
      let l = if todo.completed then "completed"::l else l in
      if todo.editing then "editing"::l else l
    in

    Html5.(li ~a:[a_class (css_class [])] [
      div ~a:[a_class ["view"]] [
        input_check ;
        label ~a:[a_ondblclick (
            fun evt -> send_event_gui (Editing_task (todo.id, true)) ; true ;
          )] [pcdata todo.Model.description] ;
        button ~a:[a_class ["destroy"]; a_onclick (
            fun evt -> send_event_gui (Delete todo.Model.id); true;
          )] []
      ];
      input_edit ;
    ])

  (** Build the tasks list *)
  let task_list visibility tasks =
    let is_visible todo =
      match visibility with
      | Model.Completed -> todo.Model.completed
      | Active -> not todo.completed
      | All -> true
    in
    let all_completed = List.for_all (fun e -> e.Model.completed) tasks in
    let css_visibility =
      match tasks with
      | [] -> "visibility: hidden;"
      | _ -> "visibility: visible;"
    in
    let toggle_input =
      Html5.(input ~a:(
          let l = [
            a_input_type `Checkbox ;
            a_class ["toggle-all"] ;
            a_onclick (fun _ ->
              send_event_gui (Check_all (not all_completed)) ; true) ;
          ] in
          if all_completed then a_checked `Checked :: l else l
        ) ())
    in
    Html5.(section ~a:[a_class ["main"] ; a_style css_visibility] [
        toggle_input ;
        label ~a:[a_for "toggle-all"] [pcdata "Mark all as complete"] ;
        ul ~a:[a_class ["todo-list"]]
          (List.rev_map todo_item (List.filter is_visible tasks))
      ])

  let visibility_swap uri visibility actual_visibility =
    let css =
      if visibility = actual_visibility then ["selected"] else []
    in
    Html5.(li ~a:[a_onclick (fun evt ->
        send_event_gui (Change_visibility visibility); true;
      )] [
        a ~a:[a_href uri; a_class css]
          [pcdata (Model.string_of_visibility visibility)]
      ])

  let controls visibility tasks =
    let open Html5 in
    let tasks_completed, tasks_left = List.partition (fun e -> e.Model.completed) tasks in
    let item = if (List.length tasks_left = 1) then " item" else " items" in
    let a_footer = [a_class ["footer"]] in
    let a_footer =
      match tasks with
      | [] -> (a_hidden `Hidden) :: a_footer
      | _ -> a_footer
    in
    let a_button = [a_class ["clear-completed"] ; a_onclick (
      fun evt -> send_event_gui Delete_complete ; true ;
    )] in
    let a_button =
      match tasks_completed with
      | [] -> (a_hidden `Hidden) :: a_button
      | _ -> a_button
    in
    let html =
      footer ~a:a_footer [
        span ~a:[a_class ["todo-count"]] [
          strong ~a:[] [pcdata (string_of_int (List.length tasks_left))];
          pcdata (item ^ " left")
        ];
        ul ~a:[a_class ["filters"]] [
          visibility_swap "#/" Model.All visibility;
          visibility_swap "#/active" Model.Active visibility;
          visibility_swap "#/completed" Model.Completed visibility;
        ];
        button ~a:a_button [
          pcdata (Printf.sprintf "Clear completed (%u)" (List.length tasks_completed))
        ]
      ]
    in
    html

  let info_footer =
    Html5.(footer ~a:[a_class ["info"]] [
        p [pcdata "Double-click to edit a todo"] ;
        p [
          pcdata "Written by " ;
          a ~a:[a_href "https://stephanelegrand.wordpress.com/"] [pcdata "Stéphane Legrand"]
        ];
        p [
          pcdata "Various code improvements from " ;
          a ~a:[a_href "https://github.com/Drup"] [pcdata "Gabriel Radanne"]
        ];
        p [
          pcdata "Based on " ;
          a ~a:[a_href "https://github.com/evancz"] [pcdata "Elm implementation by Evan Czaplicki"]
        ];
        p [
          pcdata "Part of " ;
          a ~a:[a_href "http://todomvc.com"] [pcdata "TodoMVC"]
        ]
      ])

  (** Build the HTML for the application *)
  let view m =
    let tl = task_list m.Model.visibility m.Model.tasks in
    let tl_parent = Html5.(div [tl]) in
    let _ = React.S.map (fun (visibility, tasks) ->
        replace_children (To_dom.of_div tl_parent) (To_dom.of_section (task_list visibility tasks))
      ) signal_list
    in

    let ctrl = controls m.Model.visibility m.Model.tasks in
    let ctrl_parent = Html5.(div [ctrl]) in
    let _ = React.S.map (fun (visibility, tasks) ->
        replace_children (To_dom.of_div ctrl_parent) (To_dom.of_section (controls visibility tasks))
      ) signal_controls
    in

    Html5.(
      div ~a:[a_class ["todomvc-wrapper"]] [
        section ~a:[a_class ["todoapp"]] [
          task_entry ;
          tl_parent ;
          ctrl_parent
        ];
        info_footer
      ])

  let refresh_all m =
    send_entry m.Model.field ;
    send_list (m.Model.visibility, m.Model.tasks) ;
    send_controls (m.Model.visibility, m.Model.tasks)
end

(** Manage actions, refresh view if needed and save the state in local storage *)
module Controler =
struct

  let update parent m a =
    let open Action in
    let open Model in
    let m = match a with
      | Add ->
        let uid = m.uid + 1 in
        let tasks =
          let v = String.trim m.field in
          if v = "" then m.tasks
          else (new_task v m.uid) :: m.tasks
        in
        { m with uid = uid; field = "" ; tasks = tasks }
      | Update_field field ->
        { m with field = Js.to_string field }
      | Editing_task (id, is_edit) ->
        let update_task t =
          if (t.id = id) then
            let v = String.trim t.description in
            { t with editing = is_edit ; description = v ; backup = v }
          else t
        in
        let l = List.map update_task m.tasks in
        let l = List.filter (fun e -> e.description <> "") l in
        { m with tasks = l }
      | Update_task (id, task) ->
        let update_task t =
          if (t.id = id) then { t with description = Js.to_string task }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Delete id ->
        { m with tasks = List.filter (fun e -> e.id <> id) m.tasks }
      | Delete_complete ->
        { m with tasks = List.filter (fun e -> not e.completed) m.tasks }
      | Check (id, is_compl) ->
        let update_task t =
          if (t.id = id) then { t with completed = is_compl }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Check_all is_compl ->
        let update_task t =
          { t with completed = is_compl }
        in
        { m with tasks = List.map update_task m.tasks }
      | Change_visibility visibility ->
        { m with visibility = visibility }
      | Escape id ->
        let unedit_task t =
          if (t.id = id) then { t with editing = false ; description = t.backup }
          else t
        in
        { m with tasks = List.map unedit_task m.tasks }
    in
    begin match a with
      | Update_task _ -> ()
      | Update_field _ -> send_entry m.Model.field ;
      | _ -> View.refresh_all m ;
    end ;
    Storage.set @@ Model.to_json m ;
    m
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "todomvc"))
      (fun () -> assert false)
  in
  (* restore the saved state or empty state if not found *)
  let m = Model.from_json @@ Storage.init @@ Model.to_json Model.empty in
  (* set the visibility by looking at the current url *)
  let m =
    match Url.Current.get() with
    | None -> m
    | Some u ->
      let fragment =
        match u with
        | Url.Http h
        | Url.Https h -> h.hu_fragment
        | Url.File f -> f.fu_fragment
      in
      match fragment with
      | "/" -> { m with Model.visibility = Model.All }
      | "/active" -> { m with Model.visibility = Model.Active }
      | "/completed" -> { m with Model.visibility = Model.Completed }
      | _ -> m
  in
  (* init the view *)
  let () = View.replace_children parent (Tyxml_js.To_dom.of_div (View.view m)) in
  let () = View.refresh_all m in
  (* start the application *)
  let _ = React.S.fold (Controler.update parent) m event_gui in
  Lwt.return ()

let _ = Lwt_js_events.onload () >>= main
