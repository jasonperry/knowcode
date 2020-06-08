
open Words

let st = StringyString.of_string
   
let walk = StrWord.create ~baseform:(st "walk") ~posdata:[
      Vb {tag="VB"; vbs=S; prespart=Ing; past=Ed; pastpart=Ed}
    ]
