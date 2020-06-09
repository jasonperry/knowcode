
open Words

let st = StringEx.of_string

let person = StrWord.create ~baseform:(st "person") ~lexdata:[
                 Nn {countable=true; plural=Irreg(st "people")}
               ]

let walk = StrWord.create ~baseform:(st "walk") ~lexdata:[
               Vb {vbs=S; prespart=Ing; past=Ed; pastpart=Ed};
               Nn {countable=true; plural=S}
             ]

let ride = StrWord.create ~baseform:(st "ride") ~lexdata:[
               Vb {vbs=S; prespart=Eing;
                   past=Irreg (st "rode"); pastpart=Edden};
               Nn {countable=true; plural=S}
             ]

let hide = StrWord.create ~baseform:(st "hide") ~lexdata:[
               Vb {vbs=S; prespart=Eing;
                   past=Irreg (st "hid"); pastpart=Edden};
               Nn {countable=true; plural=S}
             ]

let have = StrWord.create ~baseform:(st "have") ~lexdata:[
               Vb {vbs=Irreg (st "has"); prespart=Eing;
                   past=Irreg (st "had"); pastpart=Irreg (st "had")}
             ]

(* maybe add a "fullconj" option, but for terminological knowledge, 
 * we can do without first and second person for now. *)
let be = StrWord.create ~baseform:(st "be") ~lexdata:[
             Vb {vbs=Irreg (st "is"); prespart=Ing;
                 past=Irreg (st "was"); pastpart=Irreg (st "been")}
           ]

let all_words = [ person; walk; ride; hide; have; be ]

module VocabMap = Map.Make(StringEx) 
