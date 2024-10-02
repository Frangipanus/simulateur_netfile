open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq = 
  let (first, expr) = eq in 
  match expr with 
  |Earg(ar) |Enot(ar) |Eslice(_, _, ar)|Eselect(_, ar) -> (match ar with 
                | Avar(elem)->[ elem  ]
                |_ -> [])
  |Emux(ar1, ar2, ar3) -> (match ar1, ar2, ar3 with 
                        | Avar(f1), Avar(f2), Avar(f3) -> [ f1; f2; f3]
                        | Avar(f1), Avar(f2), Aconst(_) -> [ f1; f2]
                        | Avar(f1), Aconst(_), Avar(f2) -> [ f1; f2]
                        | Aconst(_), Avar(f1), Avar(f2) -> [ f1; f2]
                        | Aconst(_), Avar(f1), Aconst(_) -> [ f1]
                        | Aconst(_), Aconst(_), Avar(f1) -> [ f1]
                        | Avar(f1), Aconst(_), Aconst(_) -> [ f1]
                        | _ -> [] )
  |Ebinop(_, ar1, ar2) -> (match ar1, ar2 with 
                        |Avar(f1), Avar(f2)-> [f1; f2]
                        |Avar(f1), _ -> [ f1]
                        | _, Avar(f1) -> [ f1]
                        |_ -> [])
  |Eram(_,_,f2,f3,f4,f5) -> (
                               match f2 with 
                               | Avar(f) -> [f]
                               | _ -> []
                              )
  |Econcat(ar1, ar2) -> (match ar1, ar2 with 
                        |Avar(f1), Avar(f2)-> [ f1; f2]
                        |Avar(f1), _ -> [ f1]
                        | _, Avar(f1)-> [ f1]
                        |_ -> [])
  |_ -> []

  
   
              
let rec forget lst = 
  match lst with 
  |[] -> []
  |(a,b)::q -> a::(forget q)

let rec is_in elem lst = 
  match lst with 
  |[] -> false
  |h::q -> if h=elem then true else is_in elem q

let schedule p = 
  let g = mk_graph() in 
  List.iter (fun elem -> if (not(has_node g elem)) then   add_node g elem) (p.p_eqs);
  List.iter (fun equ ->   let needed = read_exp equ in 
                          List.iter (fun (elem, note) -> if (is_in elem needed) then add_edge g (elem, note) equ ) p.p_eqs   )  p.p_eqs;
  try 
    let res = topological g in 
    {p_eqs = res; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
  with
  |Cycle -> raise Combinational_cycle

  

