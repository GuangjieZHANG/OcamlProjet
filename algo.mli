(* Realize the Ford-Fulkerson Algorithm*)

open Graph

type id = string 

val find_path:  ('v, int) graph -> id -> id -> ('v, int) vertex_info list option

val find_min_flow:  ('v, int) graph -> ('v, int) vertex_info list option -> int

val toResidualGraph: ('v, int) graph -> int -> ('v, int) vertex_info list option -> unit

val calculMaxFlow: ('v, int) graph -> id -> id -> int


