

module StrMap = Map.Make(String)

(* Should I put this in the module? *)
let find_list s map =
  match StrMap.find_opt s map with
  | Some l -> l
  | None -> []

(* need separate types for role type and relation instance? 
 * or a role instance can just be a (role * concept) in the concept *)
type concept = {
    cid: string;
    mutable roles: role_inst_concept list StrMap.t
  }
and role = {         (* a role /type/. *)
    rid: string;
    domain: concept;
    range: concept;  (* DL role restrictions are only on the range? *)
    transitive: bool;
    inverse: role option
  }
(* A relation instantiated for a concept, as opposed to individual.
   In other words, a TBox role instance? *)
and role_inst_concept = {
    role: role;
    other: concept;
    default: bool
  }

type individual = {
    iid: string;
    (* class has privileged status, but it can change. 
     * Can deduce or assert more specific class. *)
    mutable myclass: concept; 
  }

(* Currently no separate type for properties; everything's a role. 
 * Property is a top-level category in the ontology. *)
(* type property = {
    pid: string;
    domain: concept
  } *)

(* will the system support adding relation types? *)

(* Assumes roles named "superclass" and "subclass" exist in ontology. *)
let rec is_subclass_of c1 c2 =
  c1 == c2 ||
  (* Depth-first search in 2 lines! *)
  find_list "subclass" c1.roles
  |> List.exists (fun {role=_; other; default=_} ->
         (* Need physical equality here, because circular references. *)
         other == c2 || is_subclass_of other c2)

let is_valid_role c1 (r: role) c2 =
  is_subclass_of c1 r.domain && is_subclass_of c2 r.range

(* Create role instance relating two concepts. *)
(* this non-validity-checking version can be called by the checking version. *)
let add_role c1 role c2 =
  c1.roles <- (
    let old_roles = find_list role.rid c1.roles
    in
    StrMap.add role.rid
      ({role=role; other=c2; default=false} :: old_roles)
      c1.roles
  )

module ConceptGraph = struct
  type t = concept list
  (* function to add concept, check for duplicate name *)
  (* function to add relation between concepts, check restrictions *)         
  let add_concept (g: t) c =
    c :: g
end

(* oh. it's meaningless to make restrictions on the subclass relation, 
 * because subclass itself is the terms in which restrictions are stated. *)

(* need a concept-maker and a relation-adder that checks everything. 
 * ...and makes indexes of relations by type *)
  
(* a concept is a node in a graph *)

(* Can i encode in the type system that superclass/subclass relations are 
 * only allowed between node of same category? *)
  (* What if I could make properties be functions and the domain and
     range be the types of categories they apply to? *)
  (* Seems like a need subclassing; I need to distinguish categories in the
   * type system, but have them be treated as same types also *)

  (* Maybe I should *not* try to use the metalanguage's type system. *)
  (* Of course, the runtime can't add new functions to the code! *)

(* category errors should be detected in the inference and flagged for 
 * feedback to the user! yeah! *)

  (* need a default and a universal form of (some) relations. *)

(* default reasoning; a property holds by default for members of a
     class, * so unless a subclass has a negation of that property it
     goes up the * hierarchy and sees if it's true. *)

