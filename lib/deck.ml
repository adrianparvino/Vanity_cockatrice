module StringMap = Map.Make(String)
type t = { 
  main: Int.t StringMap.t;
  sideboard: Int.t StringMap.t;
}

let of_string s = 
  let s = Scanf.Scanning.from_string s in
  let rec run state = function
    | s when Scanf.Scanning.end_of_input s -> state
    | s ->  
      let state' = Scanf.bscanf s "%d %[^\n]" (fun n card -> {state with main = StringMap.add card n state.main}) in
      run state' s
  in
    run { main= StringMap.empty; sideboard= StringMap.empty } s

let spread deck = StringMap.fold (fun card n acc -> List.init n (Fun.const card) @ acc) deck []
