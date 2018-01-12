open Graph

type id = string

exception No_path 

(* On a un graph *)


(* Ce fonction est pour trouver un chemin dès debut jusqu'à la fin*)
(* source sink sont ids de vetex*)
let rec find_path graph source sink =
  let new_graphe = Hashtbl.create 60 in
  (* S'il exist *)
  let source_vertex = Graph.find_vertex graph source in
  let rec aux path first_node = 
    Hashtbl.add new_graphe first_node first_node;
    let info = Graph.find_vertex graph first_node in
    let outs = info.outedges in
    let rec f = function
      | [] -> None
      | (b,a)::rest -> 
          if (||)(Hashtbl.mem new_graphe a) (b=0) then f rest
          else 
            begin 
              let a_info = find_vertex graph a in
                if (a=sink) then Some (a_info::path)
                else 
                  begin 
                    let res = aux(a_info::path) a in
                      if res = None then (f rest) else res
                  end
            end
    in
      f outs in
    aux [source_vertex] source
;;


(* This function aimes to find minimum label in path*)
let find_min_flow graph = function
  |None -> 0
  |Some x ->
      let x_rev = List.rev x in
      let rec f pos acc = function
        |[] -> acc
        |a::res -> 
            let cout = function 
              |None -> 0
              |Some x -> x
            in
            let cout_edge = cout (Graph.find_edge graph pos a.id) in
              if (&&) (cout_edge < acc) (not (String.equal pos a.id)) then f a.id cout_edge res 
              else f a.id acc res
      in
        f ((List.hd x_rev).id) 1000 x_rev
;;


let toResidualGraph graph flow = function
  |None -> ()
  |Some x ->
      let cout = function 
        |None -> 0
        |Some x -> x
      in
      let cout_edge source sink = cout (Graph.find_edge graph source sink) in
      let g source sink = Graph.add_edge graph source sink ((cout_edge source sink) - flow);
        Graph.add_edge graph source sink ((cout_edge source sink) + flow) in
      let rec f = function
        |a::b::res -> g b.id a.id ; f (b::res)
        |_ -> ()
      in f x



(* We will start calcul with two functions *)
let calculMaxFlow graph source sink =
  let rec f graph1 acc = 
    match (find_path graph1 source sink) with
      |None -> acc
      |Some x ->
          let flow = find_min_flow graph1 (Some x) in
            toResidualGraph graph1 flow (Some x);
            f graph1 (acc+flow)
  in
    f graph 0
;;
