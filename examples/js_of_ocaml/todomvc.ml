
(* Application data *)
module Model = 
struct

  type visibility = 
    Completed | Active | All
    deriving (Json)

  type task = {
    description : string;
    (* backup field keep the previous description to restore it when ESC key is pressed *)
    backup : string;
    completed : bool;
    editing : bool;
    id : int;
  } deriving (Json)

  type t = {
    tasks : task list;
    field : string;
    uid : int;
    visibility : visibility;
  } deriving (Json) (* to save/restore the state in JSON *)

  let empty () = {
    tasks = [];
    field = "";
    uid = 0;
    visibility = All;
  }

  let new_task desc id = {
    description = desc;
    backup = desc;
    completed = false;
    editing = false;
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

(* User interface actions *)
module Action = 
struct

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

let (>>=) = Lwt.bind
(* The user actions are sent in this stream *)
let (stream, (send_in_stream:(Action.action option -> unit))) = Lwt_stream.create ()

(* Build HTML and send user actions *)
module View = 
struct

  open Action
  open Tyxml_js.Html5
  open Dom_html

  (* New task input field *)
  let task_entry task = 
    let input = createInput ~_type:(Js.string "text") document in 
    let () = input##className <- (Js.string "new-todo") in
    let () = input##placeholder <- (Js.string "What needs to be done?") in
    let () = input##setAttribute(Js.string "autofocus", Js.string "autofocus") in
    let () = input##value <- Js.string task in
    let () = input##oninput <- full_handler (
      fun target evt -> send_in_stream (Some(Update_field target##value)); Js._true;
    ) in
    let () = input##onkeypress <- full_handler (
      (* key 13 is ENTER key *)
      fun target evt -> if (evt##keyCode == 13) then (send_in_stream (Some(Add))); Js._true;
    ) in
    let html = 
      header ~a:[a_class ["header"]] [
        h1 ~a:[] [ pcdata "todos" ];
        Tyxml_js.Of_dom.of_input input
      ]
    in
    html

  (* One item in the tasks list *)
  let todo_item todo = 
    let html = 
      let input_check = createInput ~_type:(Js.string "checkbox") document in 
      let () = input_check##className <- Js.string "toggle" in
      let () = input_check##checked <- Js.bool todo.Model.completed in
      let () = input_check##onclick <- full_handler (
        fun target evt -> send_in_stream (Some(Check (todo.Model.id, (not todo.Model.completed)))); Js._true;
      ) in
      let input_edit = createInput ~_type:(Js.string "text") document in 
      let () = input_edit##className <- Js.string "edit" in
      let () = input_edit##value <- Js.string todo.Model.description in
      let () = input_edit##id <- Js.string (Printf.sprintf "todo-%u" todo.Model.id) in
      let () = input_edit##oninput <- full_handler (
        fun target evt -> send_in_stream (Some(Update_task (todo.Model.id, target##value))); Js._true;
      ) in
      let () = input_edit##onblur <- full_handler (
        fun target evt -> send_in_stream (Some(Editing_task (todo.Model.id, false))); Js._true;
      ) in
      let () = input_edit##onkeypress <- full_handler (
        fun target evt -> 
          let () = 
            (* key 13 is ENTER key *)
            if (evt##keyCode == 13) then (
              send_in_stream (Some(Editing_task (todo.Model.id, false)))
            ) else (
              (* key 27 is ESC key *)
              if (evt##keyCode == 27) then (
                send_in_stream (Some(Action.Escape todo.Model.id))
              )
            )
          in
          Js._true
      ) in
      let css_class l = 
        let l = if todo.Model.completed then "completed"::l else l in
        if todo.Model.editing then "editing"::l else l
      in
      li ~a:[a_class (css_class [])] [
        div ~a:[a_class ["view"]] [
          Tyxml_js.Of_dom.of_input input_check;
          label ~a:[a_ondblclick (
              fun evt -> send_in_stream (Some(Editing_task (todo.Model.id, true))); true;
            )] [pcdata todo.Model.description];
          button ~a:[a_class ["destroy"]; a_onclick (
              fun evt -> send_in_stream (Some(Delete todo.Model.id)); true;
            )] []
        ];
        Tyxml_js.Of_dom.of_input input_edit;
      ]
    in
    html

  (* Build the tasks list *)
  let task_list visibility tasks = 
    let is_visible todo = 
      match visibility with
      | Model.Completed -> todo.Model.completed
      | Model.Active -> not todo.Model.completed
      | Model.All -> true
    in
    let all_completed = List.for_all (fun e -> e.Model.completed) tasks in
    let css_visibility = 
      match tasks with
      | [] -> "visibility: hidden;"
      | _ -> "visibility: visible;"
    in
    let input = createInput ~_type:(Js.string "checkbox") document in 
    let () = input##className <- (Js.string "toggle-all") in
    let () = input##checked <- Js.bool all_completed in
    let () = input##onclick <- full_handler (
      fun target evt -> send_in_stream (Some(Check_all (not all_completed))); Js._true;
    ) in
    let html = 
      section ~a:[a_class ["main"]; a_style css_visibility] [
        Tyxml_js.Of_dom.of_input input;
        label ~a:[a_for "toggle-all"] [pcdata "Mark all as complete"];
        ul ~a:[a_class ["todo-list"]] (List.rev_map todo_item (List.filter is_visible tasks))
      ]
    in
    html

  let visibility_swap uri visibility actual_visibility =
    let css = 
      if visibility = actual_visibility then ["selected"] else []
    in
    li ~a:[a_onclick (
      fun evt -> send_in_stream (Some(Change_visibility visibility)); true;
    )] [
          a ~a:[a_href uri; a_class css] [pcdata (Model.string_of_visibility visibility)]
    ]

  let controls visibility tasks = 
    let tasks_completed, tasks_left = List.partition (fun e -> e.Model.completed) tasks in
    let item = if (List.length tasks_left = 1) then " item" else " items" in
    let a_footer = [a_class ["footer"]] in
    let a_footer = 
      match tasks with
      | [] -> (a_hidden `Hidden) :: a_footer
      | _ -> a_footer
    in
    let a_button = [a_class ["clear-completed"]; a_onclick (
      fun evt -> send_in_stream (Some(Delete_complete)); true;
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

  let info_footer () = 
    footer ~a:[a_class ["info"]] [
      p ~a:[] [pcdata "Double-click to edit a todo"];
      p ~a:[] [
        pcdata "Written by ";
        a ~a:[a_href "https://stephanelegrand.wordpress.com/"] [pcdata "Stéphane Legrand"]        
      ];
      p ~a:[] [
        pcdata "Based on ";
        a ~a:[a_href "https://github.com/evancz"] [pcdata "Elm implementation by Evan Czaplicki"]
      ];
      p ~a:[] [
        pcdata "Part of ";
        a ~a:[a_href "http://todomvc.com"] [pcdata "TodoMVC"]
      ]
    ]

  (* Build the HTML for the application *)
  let view m = 
    let html = 
      div ~a:[a_class ["todomvc-wrapper"]] [
        section ~a:[a_class ["todoapp"]] [
          task_entry m.Model.field;
          task_list m.Model.visibility m.Model.tasks;
          controls m.Model.visibility m.Model.tasks
        ];
        info_footer ()
      ]
    in
    html

  let refresh parent m = 
    let remove_child () = 
      let rec remove () = 
        let first = parent##firstChild in
        Js.Opt.case first 
          (fun () -> ())
          (fun e -> Dom.removeChild parent e; remove())
      in
      remove ()
    in
    let () = remove_child () in
    Dom.appendChild parent (Tyxml_js.To_dom.of_div (view m))

end

(* Manage actions, refresh view if needed and save the state in local storage *)
module Controler = 
struct

  let update parent a m = 
    let open Action in
    let open Model in
    let m = 
      match a with
      | Add -> (
          let uid = m.uid + 1 in
          let tasks = 
            let v = String.trim m.field in
            if (v = "") then (
              m.tasks
            ) else (
              (new_task v m.uid) :: m.tasks
            )
          in
          { m with uid = uid; field = ""; tasks = tasks }
        )
      | Update_field field -> 
          { m with field = Js.to_string field }
      | Editing_task (id, is_edit) -> (
          let update_task t = 
            if (t.id = id) then (
              let v = String.trim t.description in
              { t with editing = is_edit; description = v; backup = v }
            ) else (
              t
            )
          in
          let l = List.map update_task m.tasks in 
          let l = List.filter (fun e -> e.description <> "") l in 
          { m with tasks = l }
        )
      | Update_task (id, task) -> (
          let update_task t = 
            if (t.id = id) then (
              { t with description = Js.to_string task }
            ) else (
              t
            )
          in
          { m with tasks = (List.map update_task m.tasks) }
        )
      | Delete id -> 
          { m with tasks = (List.filter (fun e -> e.id <> id) m.tasks) }
      | Delete_complete -> 
          { m with tasks = (List.filter (fun e -> not e.completed) m.tasks) }
      | Check (id, is_compl) -> (
          let update_task t = 
            if (t.id = id) then (
              { t with completed = is_compl }
            ) else (
              t
            )
          in
          { m with tasks = (List.map update_task m.tasks) }
        )
      | Check_all is_compl -> (
          let update_task t = 
            { t with completed = is_compl }
          in
          { m with tasks = (List.map update_task m.tasks) }
        )
      | Change_visibility visibility ->
          { m with visibility = visibility }
      | Escape id -> (
          let unedit_task t = 
            if (t.id = id) then (
              { t with editing = false; description = t.backup }
            ) else (
              t
            )            
          in
          { m with tasks = (List.map unedit_task m.tasks) }
        )
    in
    let () = 
      match a with
      | Update_field _ 
      | Update_task _ -> ()
      | _ -> View.refresh parent m
    in
    let () = 
      let window = Dom_html.window in
      let storage = window##localStorage in
      Js.Optdef.case storage
        (fun () -> ())
        (fun storage -> storage##setItem(Js.string "jsoo-todo-state", Js.string (Model.to_json m)))
    in
    m

end

let onload _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "todomvc"))
      (fun () -> assert false)
  in
  (* restore the saved state or empty state if not found *)
  let m = 
    let window = Dom_html.window in
    let storage = window##localStorage in
    Js.Optdef.case storage
      (fun () -> Model.empty())
      (fun storage -> 
        let item = storage##getItem(Js.string "jsoo-todo-state") in
        Js.Opt.case item
          (fun () -> Model.empty())
          (fun v -> Model.from_json (Js.to_string v))
      )
  in
  (* set the visibility by looking at the current url *)
  let m = 
    match Url.Current.get() with
    | None -> m
    | Some u -> (
        let fragment = 
          match u with
          | Url.Http h  
          | Url.Https h -> h.Url.hu_fragment
          | Url.File f -> f.Url.fu_fragment
        in 
        match fragment with
        | "/" -> { m with Model.visibility = Model.All }
        | "/active" -> { m with Model.visibility = Model.Active }
        | "/completed" -> { m with Model.visibility = Model.Completed }
        | _ -> m
      )
  in 
  (* init the view *)
  let () = View.refresh parent m in
  (* main loop *)
  let rec run m =
    try_lwt
      Lwt_stream.next stream >>= fun a -> (
        let m = Controler.update parent a m in
        run m;
      )
    with
    | Lwt_stream.Empty -> run m
  in
  ignore (run m);
  Js._false

let _ =
  Dom_html.window##onload <- Dom_html.handler onload
