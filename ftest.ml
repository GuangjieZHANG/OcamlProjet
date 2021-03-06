open Graph
open Algo

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in

  (* Rewrite the graph that has been read. *)
  (* Pour tester fonction export*)
  (*let () = Gfile.write_file outfile graph in*)
 
       
   let () = Gfile.export outfile graph in
   let maxFlow = Algo.calculMaxFlow (Graph.map graph int_of_string int_of_string) _source _sink in 
      
   Printf.printf "resultat %d" maxFlow;;




