open Scheduler
open Netlist_ast
open Graph


let print_only = ref false
let number_steps = ref (3)
let bool_of_int n = 
  not(n=0)
let int_of_bool s = 
  if s then 1 else 0
let not_cal value = 
  match value with 
  |VBit(e) -> VBit(not(e))
  |VBitArray(elem) -> (let res = Array.make (Array.length elem) true in 
                       for i = 0 to (Array.length elem) -1 do 
                        res.(i) <- not( elem.(i))
                       done;
                       VBitArray(res)
                       )


let cal_ope oper arg1 arg2 = 
  match oper with 
  |Or -> (match arg1, arg2 with 
          |VBit(e1), VBit(e2) -> VBit(e1 || e2) 
          |VBitArray(ar1), VBitArray(ar2)-> if(( Array.length ar1) = (Array.length ar2) ) 
                                            then begin 
                                              let res = Array.make (Array.length ar1) true in 
                                              for i = 0 to Array.length ar1 -1 do 
                                                res.(i) <- ar1.(i) || ar2.(i)
                                              done;
                                              VBitArray(res)
                                            end 
                                            else failwith "pas les bonne tailles" 
            |_ -> failwith "Les deux types dans l'opération ne sont pas compatibles")
  |Xor ->  (match arg1, arg2 with 
            |VBit(e1), VBit(e2) -> VBit(e1 <> e2) 
            |VBitArray(ar1), VBitArray(ar2)-> if(( Array.length ar1) = (Array.length ar2) ) 
                                    then begin 
                                      let res = Array.make (Array.length ar1) true in 
                                      for i = 0 to Array.length ar1 -1 do 
                                        res.(i) <- ar1.(i) <> ar2.(i)
                                      done;
                                      VBitArray(res)
                                    end 
                                    else failwith "pas les bonne tailles" 
            |_ -> failwith "Les deux types dans l'opération ne sont pas compatibles")
  |And ->  (match arg1, arg2 with 
            |VBit(e1), VBit(e2) -> VBit(e1 && e2) 
            |VBitArray(ar1), VBitArray(ar2)-> if(( Array.length ar1) = (Array.length ar2) ) 
                                    then begin 
                                      let res = Array.make (Array.length ar1) true in 
                                      for i = 0 to Array.length ar1 -1 do 
                                        res.(i) <- ar1.(i) && ar2.(i)
                                      done;
                                      VBitArray(res)
                                    end 
                                    else failwith "pas les bonne tailles" 
            |_ -> failwith "Les deux types dans l'opération ne sont pas compatibles")
  |Nand ->  (match arg1, arg2 with 
            |VBit(e1), VBit(e2) -> VBit(not(e1 && e2)) 
            |VBitArray(ar1), VBitArray(ar2)-> if(( Array.length ar1) = (Array.length ar2) ) 
                                    then begin 
                                      let res = Array.make (Array.length ar1) true in 
                                      for i = 0 to Array.length ar1 -1 do 
                                        res.(i) <- not(ar1.(i) && ar2.(i))
                                      done;
                                      VBitArray(res)
                                    end 
                                    else failwith "pas les bonne tailles" 
            |_ -> failwith "Les deux types dans l'opération ne sont pas compatibles")
  

let concat arr1 arr2 = 
  let len1 = Array.length arr1 in 
  let len2 = Array.length arr2 in 
  let res = Array.make (len1 + len2) true in
  for i = 0 to len1 -1 do 
    res.(i) <- arr1.(i)
  done;
  for i = len1 to len1 + len2 -1 do 
    res.(i) <- arr2.(i-len1)
  done;
  res

let slice arr ind1 ind2 = 
  let res = Array.make (ind2 - ind1 + 1) true in 
  for i = ind1 to ind2 do 
    res.(i-ind1) <- arr.(i)
  done;
  res


let array_to_int arr= 
  let pow = ref 1 in 
  let res = ref 0 in 
  for i = Array.length arr -1 downto 0 do 
   res := !res + !pow*(int_of_bool arr.(i)); 
   pow := !pow*2
  
  done; 
  !res

let deal_with_exp exp table types registre rams roms  = 
  let (name, eq) = exp in 
  

  match eq with 
  |Earg(elem) -> (match elem with 
                  |Avar(id) -> Hashtbl.add table name (Hashtbl.find table id) 
                  |Aconst(value)-> Hashtbl.add table name value)
  |Enot(elem) -> (match  elem with
                | Aconst(value) -> Hashtbl.add table name (not_cal value) 
                | Avar(id) ->  Hashtbl.add table name (not_cal (Hashtbl.find table id) ) ) 
  |Ebinop(ope, arg1, arg2) -> (match arg1, arg2 with 
                                | Avar(id1), Avar(id2) -> Hashtbl.add table name (cal_ope ope (Hashtbl.find table id1) (Hashtbl.find table id2))
                                | Avar(id1), Aconst(val2) -> Hashtbl.add table name (cal_ope ope (Hashtbl.find table id1) val2)
                                | Aconst(val1), Avar(id1)-> Hashtbl.add table name (cal_ope ope (Hashtbl.find table id1) val1)
                                |Aconst(val1), Aconst(val2) -> Hashtbl.add table name (cal_ope ope val1 val2)
                                  )
  | Emux (choice, arg1, arg2) -> (
                                  match choice with 
                                  |Avar(id) -> (let decision = 
                                                match Hashtbl.find table id  with 
                                                |VBit(s) -> s
                                                |_ ->failwith "choice too big"
                                                in 
                                                if decision then (
                                                  match arg2 with
                                                    |Avar(id) -> Hashtbl.add table name (Hashtbl.find table id) 
                                                    |Aconst(value)-> Hashtbl.add table name value)

                                                  else (
                                                    match arg1 with 
                                                    |Avar(id) -> Hashtbl.add table name (Hashtbl.find table id) 
                                                    |Aconst(value)-> Hashtbl.add table name value)
                                                  ) 
                                    
                          
                                  | Aconst(value) -> ( let decision = 
                                                match  value  with 
                                                |VBit(s) -> s
                                                |_ ->failwith "choice too big"
                                              in 
                                              if decision then (
                                                match arg2 with
                                                  |Avar(id) -> Hashtbl.add table name (Hashtbl.find table id) 
                                                  |Aconst(value)-> Hashtbl.add table name value)
                                              
                                                else (
                                                  match arg1 with 
                                                  |Avar(id) -> Hashtbl.add table name (Hashtbl.find table id) 
                                                  |Aconst(value)-> Hashtbl.add table name value)
                                                                        )
  )
  |Econcat(arg1, arg2) -> (let (arr1, arr2) = match arg1, arg2 with 
                              |Avar(id1), Avar(id2) -> (Hashtbl.find table id1, Hashtbl.find table id2) 
                              | Avar(id1), Aconst(val1) -> (Hashtbl.find table id1, val1)
                              |Aconst(val1), Avar(id1) -> (val1, Hashtbl.find table id1)
                              | Aconst(val1), Aconst(val2 ) -> (val1, val2)
                                              in 
                            match  arr1, arr2 with
                            |VBitArray(n1), VBitArray(n2)  -> Hashtbl.add table name (VBitArray(concat n1 n2))
                            |VBitArray(n1), VBit(s) -> Hashtbl.add table name (VBitArray(concat n1 [|s|]))
                            |VBit(s), VBitArray(n2) -> Hashtbl.add table name (VBitArray(concat [|s|] n2))
                            |VBit(s1), VBit(s2) -> Hashtbl.add table name (VBitArray([|s1; s2|]))
                            )
                                  
  |Eslice (ind1, ind2, arg) -> (match arg with 
                              |Avar(id1) -> (match Hashtbl.find table id1 with 
                                            |VBitArray(n) -> (Hashtbl.add table name (VBitArray(slice n ind1 ind2)))
                                            |_ -> failwith "pas un array on peut pas le slice")  
                              |Aconst(val1) ->(match val1 with 
                                            |VBitArray(n) -> Hashtbl.add table name (VBitArray(slice n ind1 ind2))
                                            |_ -> failwith "pas un array on peut pas le slice")   )
  |Eselect(ind1, arg) -> (match arg with 
                          |Avar(id1) -> (match Hashtbl.find table id1 with 
                                          |VBitArray(n) -> Hashtbl.add table name (VBit(n.(ind1)))
                                          |VBit(s) -> Hashtbl.add table name (VBit(s)) )
                          |Aconst(val1) -> (match val1 with 
                                            |VBitArray(n) -> Hashtbl.add table name (VBit(n.(ind1)))
                                            |VBit(s) -> Hashtbl.add table name (VBit(s)))   )
  |Ereg(x) -> Hashtbl.add table name (Hashtbl.find registre x)
  |Erom(addr_size, word_size, arg) -> (let addr1 = (match arg with 
                                            |Avar(id1) -> (Hashtbl.find table id1 )
                                                |Aconst(value) -> value
                                              )
                                                  in 
                                              let addr = match addr1 with 
                                              |VBit(s) -> [|s|]
                                              |VBitArray(n) -> n in 
                                                  Hashtbl.add table name ((Hashtbl.find roms name).(array_to_int addr))
                                        

                                             )
                                            
  
  |Eram (addr_size, word_size, read_addr, write_enable, write_addr, data) -> (
  
                                                                    let ra1 = match read_addr with
                                                                    | Avar(id1) -> Hashtbl.find table id1 
                                                                    |Aconst(val1) -> val1
                                                                    in 
                                                                    let ra = match ra1 with 
                                                                    |VBit(s) -> [|s|]
                                                                    |VBitArray(n) -> n 
                                                                    in 
                                                                    Hashtbl.add table name ((Hashtbl.find rams name).(array_to_int ra));
                                                                  
                                                                    

                                                  )

let rec expo x n = 
  if n = 0 then 1 else
  x*(expo x (n-1))

let array_creator word_size = 
  VBitArray((Array.make word_size false))
let init_ram add_size word_size = 
  let res = Array.make (expo 2 add_size) (array_creator word_size) in 
  for i = 0 to Array.length res -1 do
    res.(i) <- array_creator word_size
  done; 
  res

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


let simulator program number_steps = 
  let types = program.p_vars in 
  let prog_order = schedule program in 
  let variables = Hashtbl.create 42 in 
  let registres = Hashtbl.create 42 in 
  let rams = Hashtbl.create 42 in 
  let roms = Hashtbl.create 42 in 
  List.iter(fun (name, elem) -> match elem with 
                          |Ereg(x) -> Hashtbl.add registres x (VBit(false))
                          |Erom(add_size,word_size, _) -> Hashtbl.add roms name (init_rom add_size word_size (name ^ ".db"))
                          |Eram(add_size, word_size, _,_,_,_) -> (Printf.printf " %s \n" name; Hashtbl.add rams name (init_ram add_size word_size))
                          |_ -> ()) program.p_eqs;

  for i = 0 to number_steps -1 do 
    Printf.printf "Now starting step %d\n" i;
   
    List.iter (fun elem ->let length = 
                match (Env.find  elem types ) with 
                  |TBit -> 1
                  |TBitArray(len) -> len 
                  in 
                let ndone = ref true in 
                while !ndone do  
                  Printf.printf "\nDonnder la valeur de %s qui a une longeur %d: " elem length;
                  let s = read_line() in 
                  
                  if (String.length ( s) = length) 
                  then (ndone := false;
                        if (length = 1) then 
                        Hashtbl.add variables elem (VBit((bool_of_int (int_of_string s))))
                      else begin 
                        let acc = Array.make length true in 
                        let st = (s) in 
                        for i = 0 to length -1 do 
                          acc.(i) <- bool_of_int (int_of_string (String.make 1 st.[i]));
                        done;
                        Hashtbl.add variables elem (VBitArray(acc))
                      end)
                                        
                  else Printf.printf "Wrong size\n"
                done
      ) prog_order.p_inputs;
    List.iter (fun exp -> deal_with_exp exp variables types registres rams roms ) prog_order.p_eqs;
 
    List.iter (fun (name, elem) -> match elem with (*Met a jour les rams*) 
                    |Eram (addr_size, word_size, read_addr, write_enable, write_addr, data) -> (let we1 = match write_enable with
                                                                    | Avar(id1) -> Hashtbl.find variables id1 
                                                                    |Aconst(val1) -> val1
                                                                    in 
                                                                    let we = match we1 with 
                                                                    |VBit(s) -> s
                                                                    |VBitArray(n) -> failwith "pas un bit mais un tableau"
                                                                    in 
                                                                   
                                                                    let wa1 = match write_addr with
                                                                    | Avar(id1) -> Hashtbl.find variables id1 
                                                                    |Aconst(val1) -> val1
                                                                    in 
                                                                    let wa = match wa1 with 
                                                                    |VBit(s) -> [|s|]
                                                                    |VBitArray(n) -> n
                                                                    in 
                                                                    
                                                                    let data1 = match data with
                                                                    | Avar(id1) -> (Hashtbl.find variables id1 )
                                                                    |Aconst(val1) -> val1
                                                                    in 
                                                                    
                                                                    let dat = match data1 with 
                                                                    |VBit(s) -> VBitArray([|s|])
                                                                    |VBitArray(n) -> VBitArray(n)
                                                                    in 
                                                                    
                                                                    
                                                                    if we then ((Hashtbl.find rams name).(array_to_int wa) <- dat)
                                                                    else () 
)
                          |_ -> ()
 
    ) prog_order.p_eqs;
    List.iter (fun id -> match Hashtbl.find variables id with 
                          |VBit(s) -> Printf.printf "%s = %d\n" id (int_of_bool s)
                          |VBitArray(s) -> (Printf.printf "%s = " id; Array.iter (fun x-> Printf.printf("%d") (int_of_bool x)) s; Printf.printf "\n")) prog_order.p_outputs;
    List.iter(fun (name, elem) -> match elem with (*Met a jour les registres*)
                          |Ereg(x) -> Hashtbl.add registres x (Hashtbl.find variables x)
                          |_ -> ()) prog_order.p_eqs;
    
    
  done

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
