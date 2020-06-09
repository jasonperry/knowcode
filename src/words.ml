
(* Data structures for word syntax/semantics representation *)

(* Later: make these different for different languages? *)
type 's pluralform =
  | S
  | Es
  | Yies
  | Irreg of 's

type 's pastform =
  | Ed
  | Dded (* consonant doubling: "clapped" *)
  | Yied (* change y to i *)
  | Edden (* for the past participle "ridden" *)
  | Irreg of 's

type 's ingform =
  | Ing
  | Dding (* consonant doubling *)
  | Eing  (* drop the e *)
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

type 's nouninfo = { countable: bool;
                     plural: 's pluralform } (* Gerund? Infinitive? *)

(* thinking that different forms of a verb can be in one record. *)
type 's verbinfo = { vbs: 's pluralform;
                     prespart: 's ingform;
                     past: 's pastform;
                     pastpart: 's pastform }

(* Still need a dictionary to POS lookup of a word *)
(* Actually, a datatype for a word with specific POS *)
type 's lexinfo =
  | Nn of 's nouninfo
  | Vb of 's verbinfo
  | Jj
  | Rb
  | Prep
  | Pron
  | Conj
  | Det
           
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
  val show : t -> string
  val append : t -> t -> t
  val append_r : t -> t -> t
  val compare : t -> t -> int
end

(* This is more of the "dictionary" type, supporting extraction of 
 * information. *)
module Word = functor (St: STRINGY) -> struct
  module PosMap = Map.Make(St)
  module StSet = Set.Make(St)
  type t = {
      baseform: St.t;
      lexdata: St.t lexinfo list;
      (* posdict: St.t lexinfo PosMap.t; (* gen_entry takes care... *)
      allforms: StSet.t *)
    }
  type sentence = (St.t * t) list
  let create ~baseform ~lexdata = {
      baseform;
      lexdata;
      (* posdict = PosMap.empty;
      allforms = StSet.empty *)
    }
  (* Parse from JSON/Xml? *)
  let rtrim s n = St.sub s 0 (St.length s - n)
  let to_string w = w.baseform  (* just a hack to print the base form *)
  let pluralize baseform pltype = match pltype with
    | S -> St.append baseform (St.of_string "s")
    | Es -> St.append baseform (St.of_string "es")
    | Yies -> St.append (rtrim baseform 1) (St.of_string "ies")
    | Irreg s -> s
  let add_ing baseform ingtype = match ingtype with
    | Ing -> St.append baseform (St.of_string "ing")
    | Dding ->
       let dcons = St.sub baseform (St.length baseform - 1) 1
       in
       St.append baseform dcons
       |> St.append_r (St.of_string "ing")
    | Eing -> St.append (rtrim baseform 1) (St.of_string "ing")
    | Irreg s -> s
  let add_past baseform pastform = match pastform with
    | Ed -> St.append baseform (St.of_string "ed")
    | Dded ->
       let dcons = St.sub baseform (St.length baseform - 1) 1
       in
       St.append baseform dcons
       |> St.append_r (St.of_string "ed")
    | Yied -> rtrim baseform 1
              |> St.append_r (St.of_string "ied")
    | Edden ->
       let len = St.length baseform in
       let dcons = St.sub baseform (len-2) 1
       in
       St.append (rtrim baseform 1) dcons
       |> St.append_r (St.of_string "en")
    | Irreg s -> s
  (* Generate all lexicon entries from string to (string, POS, t) triples 
   * (originally just lexinfo, but t has the base in it too, so good) *)
  let gen_entry w =
    w.lexdata
    |> List.map (function
           | Nn { countable; plural } ->
              if countable then [
                  (w.baseform, "NN-S", w);
                  (pluralize w.baseform plural, "NN-P", w)
                ]
              else []
           | Vb { vbs; prespart; past; pastpart } -> [
               (w.baseform, "VP-S", w);
               (pluralize w.baseform vbs, "VB-P", w);
               (add_ing w.baseform prespart, "VB-G", w);
               (add_past w.baseform past, "VB-D", w);
               (add_past w.baseform pastpart, "VB-N", w)
             ]
           | Jj -> [ (w.baseform, "JJ-S", w) ]
           | Rb -> [ (w.baseform, "RB-S", w) ]
           | Conj -> [ (w.baseform, "CJ", w) ]
           | Det -> [ (w.baseform, "DT", w) ]
           | Prep -> [ (w.baseform, "IN", w) ]
           | Pron -> [ (w.baseform, "PN", w) ]
         )
    |> List.concat
end

(* String-specific module implementations start here. *)
                                     
module StringEx : STRINGY = struct
  include String
  type c = char
  let of_string s = s
  let show s = s
  let append s1 s2 = s1 ^ s2
  let append_r s1 s2 = s2 ^ s1
end

(* For the toplevel pretty-printer *)
let format_stringex fmt ss =
  Format.fprintf fmt "\"%s\"" (StringEx.show ss)

                               
(* Eventually want to parameterize by the language too...or,
 * Just have a different Word module of same signature *)
module StrWord = Word(StringEx)

(* Assumes word also includes punctuation. *)
type sentence = (string * StrWord.t) list
(* no way, right? I'd have to put the sentence type in a module. Oh, OK. 
 * do that. it's not a class!! 
 * So all the types parameterized by the functor argument should be in the functor? 
 * I guess that makes sense. *)
(* type 's sentence = (STRINGY.t * Word.t) list *) 
let () = print_endline "Hello, word world."

