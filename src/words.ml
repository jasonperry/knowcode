
(* Data structures for word syntax/semantics representation *)

(* Later: make these different for different languages? *)
type 's pluralform =
  | S
  | Es
  | Yies
  | Irreg of 's

type 's pastform =
  | Ed
  | Dded (* consonant doubling *)
  | Yied
  | Irreg of 's

type 's ingform =
  | Ing
  | Dding
  | Eing  (* what was this for? *)
  | Irreg of 's
           
(* Need to store noun and verb /types/ also: transitive/intransitive,
 * action/state of being -- or is that in the ontology only?
 *)
  
(* Nn of postag * nouninfo? 
 * still want to match at varying degrees of specificity. Do it
 * stringily, with prefixes? 
 * Parse by matching just structure first with top-level tags, then trying
 * More specifics, backtracking if none work?
 *)
(* Just put the tag string in the record, and when I make the map, I just
 * pull it out and add the tag separately. That's how a map works anyway. *)

type 's nouninfo = { tag: string;
                     countable: bool;
                     plural: 's pluralform } (* Gerund? Infinitive? *)

(* thinking that different forms of a verb can be in one record. *)
type 's verbinfo = { tag: string;
                     vbs: 's pluralform;
                     prespart: 's ingform;
                     past: 's pastform;
                     pastpart: 's pastform }

(* Still need a dictionary to POS lookup of a word *)
(* Actually, a datatype for a word with specific POS *)
type 's posinfo =
  | Nn of 's nouninfo
  | Vb of 's verbinfo
  | Jj
  | Rb
           
(* idea: word module parameterized by a module that has a 
 * "string" type that supports character indexing. This way we can support 
 * any fancy string type. *)

(* I need some kind of index from strings to all the forms they represent. *)
           
module type STRINGY = sig
  type t
  type c
  val get : t -> int -> c
  val length : t -> int
  val sub : t -> int -> int -> t
  val of_string : string -> t
  val append : t -> t -> t
  val compare : t -> t -> int
end

(* This is more of the "dictionary" type, supporting extraction of 
 * information. *)
module Word = functor (St: STRINGY) -> struct
  module PosMap = Map.Make(St)
  module StSet = Set.Make(St)
  type t = {
      baseform: St.t;
      posdata: St.t posinfo list;
      posdict: St.t posinfo PosMap.t;
      allforms: StSet.t
    }
  type sentence = (St.t * t) list
  let create ~baseform ~posdata = {
      baseform; posdata;
      posdict = PosMap.empty;
      allforms = StSet.empty
    }
  (* Parse from JSON/Xml? *)
  let rtrim s n = St.sub s 0 (St.length s - n)
  let to_string w = w.baseform  (* maybe not actual string *)
  (* let add_plural w pl = match pl with
    | S -> { w with plural = St.append w.baseform (St.of_string "s") }
    | Es -> { w with plural = St.append w.baseform (St.of_string "es") }
    | Yies -> { w with
                plural = St.append (rtrim w.baseform 1) (St.of_string "ies") }
    | Irreg s -> { w with plural = s } *)
end

(* String-specific module implementations start here. *)
                                     
module StringyString : STRINGY = struct
  include String
  type c = char
  let of_string s = s
  let append s1 s2 = s1 ^ s2
end

(* Eventually want to parameterize by the language too...or,
 * Just have a different Word module of same signature *)
module StrWord = Word(StringyString)

(* Assumes word also includes punctuation. *)
type sentence = (string * StrWord.t) list
(* no way, right? I'd have to put the sentence type in a module. Oh, OK. 
 * do that. it's not a class!! 
 * So all the types parameterized by the functor argument should be in the functor? 
 * I guess that makes sense. *)
(* type 's sentence = (STRINGY.t * Word.t) list *) 
let () = print_endline "Hello, word world."

