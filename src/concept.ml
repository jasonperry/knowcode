(** A graph representation of a DL-like knowledge base. *)

module StrMap = Map.Make(String)

(* Should I put this in a 'common' file? *)
let find_list s map =
  match StrMap.find_opt s map with
  | Some l -> l
  | None -> []


type concept = {
    cid: string;
    (* Map from role name to list of role instances of that type. *)
    mutable roles: role_inst_concept list StrMap.t
  }

(** a role /type/. *)
and role = {         
    rid: string;
    domain: concept;
    range: concept;  (* DL role restrictions are only on the range? *)
    transitive: bool;
    inverse: role option
  }

(** A relation instantiated for a concept, as opposed to individual.
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
    (* just an idea, don't know if this is the final representation. *)
    (* List might be better than map because anyway we can have
       multiple targets for a given role. *)
    mutable iroles: (role * [`Con of concept | `Ind of individual]) list
  }

(** Get list of all roles of a given type for an individual. *)
let individual_get_roles ind rtype =
  List.filter
    (fun (r, _) -> r.rid = rtype)
    ind.iroles

(* will the system support adding relation types? *)

(* Assumes roles named "superclass" and "subclass" exist in
       ontology. *)
let rec concept_is_subclass_of c1 c2 =
  c1 == c2 ||
    (* Depth-first search in 2 lines! *)
    find_list "subclass" c1.roles
    |> List.exists
         (fun {role=_; other; default=_} ->
           (* Need physical equality here, because circular references. *)
           other == c2 || concept_is_subclass_of other c2)

(** Check that a role is allowed by domain/range restrictions. *)
let concept_is_valid_role c1 (r: role) c2 =
  concept_is_subclass_of c1 r.domain && concept_is_subclass_of c2 r.range

(** Create role instance relating two concepts. *)
(* this non-validity-checking version can be called by the checking version. *)
let concept_add_role c1 role c2 =
  c1.roles <- (
    let old_roles = find_list role.rid c1.roles in
    StrMap.add
      role.rid
      ({role=role; other=c2; default=false} :: old_roles)
      c1.roles
  )

(** Get list of roles of a given type for a concept. *)
let concept_get_roles c rname = find_list rname c.roles


module ConceptGraph = struct
  type t = {
      (* Eventually make it mutable *)
      cmap: concept StrMap.t;
      rmap: role StrMap.t;
    }

  let create clist rlist = {
      cmap = List.fold_left
               (fun map concept -> StrMap.add concept.cid concept map)
               StrMap.empty
               clist;
      rmap = List.fold_left
               (fun map role -> StrMap.add role.rid role map)
               StrMap.empty
               rlist;
    }

  let get_concept graph cname = StrMap.find_opt cname graph.cmap
  let get_role graph rname = StrMap.find_opt rname graph.rmap
  (* Should inference function corresponding to questions. go here? *)
  (* function to add concept, check for duplicate name *) 
  (* function to add relation between concepts, check restrictions *)
  (* let add_concept_under graph concept parent =  *)
  (* let get_concepts_with_role graph role target *)
end

                        (**/**)

(* need a concept-maker and a relation-adder that checks everything.
       * ...and makes indexes of relations by type *)

(* Can i encode in the type system that superclass/subclass relations
       are * only allowed between node of same category? *) (* What if
       I could make properties be functions and the domain and range
       be the types of categories they apply to? *) (* Seems like a
       need subclassing; I need to distinguish categories in the *
       type system, but have them be treated as same types also *)

(* Maybe I should *not* try to use the metalanguage's type
       system. *) (* Of course, the runtime can't add new functions to
       the code! *)

(* category errors should be detected in the inference and flagged for
       * feedback to the user! yeah! *)

  (* need a default and a universal form of (some) relations. *)

(* default reasoning; a property holds by default for members of a
       class, * so unless a subclass has a negation of that property
       it goes up the * hierarchy and sees if it's true. *)

