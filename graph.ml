exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let has_node g x = 
  List.exists (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes


let has_cycle g = 
  clear_marks g;
  let rec aux som = 
    match som.n_mark with 
    | Visited -> false 
    | InProgress -> true
    | NotVisited -> ( som.n_mark <- InProgress; 
                    let res = List.exists (fun x-> aux x ) som.n_link_to in 
                    som.n_mark <- Visited;
                    res)
  in 
  let res = List.exists (fun x-> aux x) g.g_nodes in 
  clear_marks g;
  res



let topological g = 
  clear_marks g;
  if has_cycle g then raise Cycle 
  else begin 
    clear_marks g;
  let rec aux som lst = 
    match som.n_mark with 
    |Visited -> lst
    | NotVisited -> ( som.n_mark <- InProgress; let acc =  (som.n_label)::(List.fold_left( fun accu elem -> aux elem accu) lst som.n_link_to) in som.n_mark <- Visited; acc  )
    | InProgress -> raise Cycle
  in 
  let res =  List.fold_left (fun acc elem -> (aux elem acc)) [] g.g_nodes in 
  res
  end