(** Upper ontology
  * These concepts and roles may be assumed in inference methods. *)

open Concept

let universal_concept: concept = { cid="Concept"; roles=StrMap.empty }
let entity: concept = { cid="Entity"; roles=StrMap.empty }
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
    rid = "disjointWith";
    domain = universal_concept;
    range = universal_concept;
    transitive = false;
    inverse = Some disjoint_with  (* it's own inverse! Yeah! *)
  }

let has_property: role = {
    rid = "hasProperty";
    domain = entity;
    range = property;
    transitive = false;
    inverse = None
  }

(* entity <-> universal *)
let () = concept_add_role universal_concept superclass entity
let () = concept_add_role entity subclass universal_concept

(* physical <-> entity *)
let () = concept_add_role entity superclass physical_object
let () = concept_add_role physical_object subclass entity

(* abstract <-> entity *)
let () = concept_add_role entity superclass abstract_object
let () = concept_add_role abstract_object subclass entity

(* property <-> universal *)
let () = concept_add_role universal_concept superclass property
let () = concept_add_role property subclass universal_concept

(* Disjointness of all the top categories *)
let () = concept_add_role physical_object disjoint_with abstract_object
let () = concept_add_role abstract_object disjoint_with physical_object

let () = concept_add_role property disjoint_with physical_object
let () = concept_add_role physical_object disjoint_with property

let () = concept_add_role property disjoint_with abstract_object
let () = concept_add_role abstract_object disjoint_with property

let upper_ontology =
  ConceptGraph.create
    [ universal_concept;
      entity;
      physical_object;
      abstract_object;
      property
    ]
    [ superclass;
      subclass;
      disjoint_with;
      has_property
    ]

