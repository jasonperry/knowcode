
open Concept

(* start by specifying the root ontology in code.*)
let universal_concept: concept = { cid="Concept"; roles=StrMap.empty }
let physical_object: concept = { cid="Physical_object"; roles=StrMap.empty }
let abstract_object: concept = { cid="Abstract_object"; roles=StrMap.empty }
let property: concept = { cid="Property"; roles=StrMap.empty }
(* let action: concept = *)

let rec subclass: role = {
    rid="subclass";
    domain = universal_concept;
    range = universal_concept;
    transitive = true;
    inverse = Some superclass
  }
and superclass: role = {
    rid = "superclass";
    domain = universal_concept;
    range = universal_concept;
    transitive = true;
    inverse = Some subclass
  } 

let rec disjoint_with: role = {
    rid = "disjoint_with";
    domain = universal_concept;
    range = universal_concept;
    transitive = false;
    inverse = Some disjoint_with  (* it's own inverse! Yeah! *)
  }

let () = add_role universal_concept superclass physical_object
let () = add_role physical_object subclass universal_concept

let () = add_role universal_concept superclass abstract_object
let () = add_role abstract_object subclass universal_concept

let () = add_role universal_concept superclass property
let () = add_role property subclass universal_concept

(* Disjointness of all the top categories *)
let () = add_role physical_object disjoint_with abstract_object
let () = add_role abstract_object disjoint_with physical_object

let () = add_role property disjoint_with physical_object
let () = add_role physical_object disjoint_with property

let () = add_role property disjoint_with abstract_object
let () = add_role abstract_object disjoint_with property
