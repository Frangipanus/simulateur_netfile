open Netlist_ast
let bool_of_int n = 
  not(n=0)
let array_creator word_size = 
  VBitArray((Array.make word_size false))
let rec expo x n = 
  if n = 0 then 1 else
  x*(expo x (n-1))
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
let init_rom add_size word_size filename = 
  let res = Array.make (expo 2 add_size) (array_creator word_size) in 
  let lines = read_file filename in 
  let rec aux lst ind = 
    match lst with 
    |[] -> ()
    |h::q -> begin 
        let acc = Array.make word_size false in 
        for i = 0 to word_size - 1 do 
          acc.(i) <- bool_of_int (int_of_string(String.make 1 (h.[i])))
        done;
        res.(ind) <- VBitArray(acc);
        aux q (ind+1)
    end
  in 
  aux lines 0;
  res


