open Circuit ;;
    
let draw_raw print_string entrees sorties =
  let (types,portes_nand,portes_delai,entrees,sorties,_) = compile_raw entrees sorties in
  let nb = Hashtbl.create 17 in
  let _ = Array.iteri (fun i e -> Hashtbl.add nb e i) entrees in
  let _ = Array.iteri (fun i e -> Hashtbl.add nb e i) sorties in
  
  let name_delai a b =
    "DELAI_" ^ (string_of_int a) ^ "_" ^ (string_of_int b)
  in
  let name_nand a b c =
    "NAND_" ^ (string_of_int a) ^ "_" ^ (string_of_int b)^ "_" ^ (string_of_int c)
  in
  let name_node a =
    "NODE_" ^ (string_of_int a)
  in
  print_string "digraph { \n" ;

  for i = 0 to Array.length types - 1 do
    name_node i ^ (match types.(i)
                   with
                   | ENTREE -> " [shape=cds,label=\"In "^(string_of_int @@ Hashtbl.find nb i)^"\"]"
                   | ZERO -> " [label=\"0\"]"
                   | UN ->  " [label=\"1\"]"
                   | _ ->
                      if Hashtbl.mem nb i
                      then "[shape=star,label=\"Out "^(string_of_int @@ Hashtbl.find nb i)^"\"]"
                      else "[shape=point,label=\"\"]"
                  ) ^ " ;\n" |> print_string
  done ;

  if portes_delai <> []
  then
  begin
    print_string "{ node [shape=box] ; \n " ;
    List.iter (fun (a,b) ->  ((name_delai a b) ^ " [label=\"D\"] ;\n") |> print_string) portes_delai ;
    print_string "}\n\n" ;
    List.iter (fun (a,b) ->  ((name_delai a b) ^ " -> { " ^ (name_node b) ^ " }\n") |> print_string) portes_delai ;
    List.iter (fun (a,b) ->  ((name_node a) ^ " -> { " ^ (name_delai a b) ^ " }\n") |> print_string) portes_delai ;
  end ;
  if portes_nand <> []
  then
  begin
    print_string "{ node [shape=diamond] ; \n " ;
    List.iter (fun (a,b,c) ->  ((name_nand a b c) ^ " [label=\"N\"] ;\n") |> print_string) portes_nand ;
    print_string "}\n\n" ;
    List.iter (fun (a,b,c) ->  ((name_nand a b c) ^ " -> { " ^ (name_node c) ^ " }\n") |> print_string) portes_nand ;
    List.iter (fun (a,b,c) ->  ((name_node a) ^ " -> { " ^ (name_nand a b c) ^ " }\n") |> print_string) portes_nand ;
    List.iter (fun (a,b,c) ->  ((name_node b) ^ " -> { " ^ (name_nand a b c) ^ " }\n") |> print_string) portes_nand ;
  end  ;
  
  
  print_string "} \n" 

let draw =
  let (_, sin, _)  = Unix.open_process_full "dot -Tpng | display" (Unix.environment ()) in
  let fmt = Format.formatter_of_out_channel sin in
  let print_string = Format.fprintf fmt "%s@."  in
  draw_raw print_string

let draw_pdf filename =
  let (_, sin, _)  = Unix.open_process_full ("dot -Tpdf -o "^filename) (Unix.environment ()) in
  let fmt = Format.formatter_of_out_channel sin in
  let print_string = Format.fprintf fmt "%s@."  in
  fun e s -> draw_raw print_string e s ; Unix.sleep 2 
