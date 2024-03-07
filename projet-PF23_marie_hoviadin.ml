(*HOVIADIN Oleksandr 21109266
  MARIE Clement 21117891 *)

(*
trace 1:
9 12 15 TUCK pour TUCK DUP ROT SWAP
avant appel: 
9 12 15
apres: 9 15 12 15

trace 2:
CUBE DUP CARRE *; CARRE DUP *
pour 6 CUBE
avant appel: 6
apres: 216
*) 


(* *********** Question 1.a *********** *)

type element = 
  | OpPile of string
  | OpAri of string
  | OpComp of string
  | Mot of string
  | CstInt of int
  | CstBool of bool ;;

(* *********** Question 1.b *********** *)

let to_string (x:element) : string = 
  match x with
  | OpPile s -> s
  | OpAri s -> s
  | OpComp s -> s
  | Mot s -> s
  | CstInt i -> string_of_int i
  | CstBool b -> String.uppercase_ascii (string_of_bool b) ;;

let of_string (s:string) : element =
  try
    CstInt (int_of_string s)
  with
  | Failure _ -> 
      match s with
      | "DUP" -> OpPile s
      | "DROP" -> OpPile s
      | "SWAP" -> OpPile s
      | "ROT" -> OpPile s
      | "*" -> OpAri s
      | "/" -> OpAri s
      | "+" -> OpAri s     
      | "-" -> OpAri s
      | "=" -> OpComp s
      | "<>" -> OpComp s
      | "<" -> OpComp s
      | ">" -> OpComp s
      | "TRUE" -> CstBool true
      | "FALSE" -> CstBool false
      | _ -> Mot s ;;


(* *********** Question 1.c *********** *)

(** fonction utilitaire : 
    [split s] découpe le texte [s] en une suite de mot. 
*)
let split (s:string) : string list =
  (* traite les tabulations et les sauts de lignes comme des espaces *)
  let normalize_s = String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s in
  let split_s = String.split_on_char ' ' normalize_s in
  (* ignore les espaces successifs *)
  List.filter ((<>) "") split_s ;;

assert (split "  \t  \n " = []) ;;
assert (split " A     \n B\t C  " = ["A";"B";"C"]) ;;

(** transforme un texte (représentant un programme ou une pile)
    en une suite de symboles du langage (e.g., "42" et "DUP") 
*)
let parse (s:string) : element list =
  let split_liste = split s in
  List.map of_string split_liste ;;

(** transforme un suite de symbole du langage (représentant un programme 
    ou une pile) en un texte équivalent. 
    Par exemple : [text (parse "1 2 +")) = "1 2 +"].
*)
let text (p:element list) : string =
  let new_liste = List.map to_string p in
  let rec loop (acc : string) (liste : string list) =
    match liste with 
    | [] -> acc
    | a::l -> if acc = "" then loop (acc ^ a) l
        else loop (acc ^ " " ^ a) l
  in
  loop "" new_liste ;;

(* *********** Question 2 *********** *)

type prog = element list
type stack = element list

(* fonction auxiliaire : évaluation d'un opérateur binaire *)
let eval_binop (op : string) (e1:element) (e2:element) : element =
  match op, e1, e2 with
  | "+", CstInt x, CstInt y -> CstInt (x + y)
  | "-", CstInt x, CstInt y -> CstInt (x - y)
  | "*", CstInt x, CstInt y -> CstInt (x * y)
  | "/", CstInt x, CstInt y -> CstInt (x / y)
  | "=", CstInt x, CstInt y -> CstBool (x = y)
  | "<>", CstInt x, CstInt y -> CstBool (x <> y)
  | "<", CstInt x, CstInt y -> CstBool (x < y)
  | ">", CstInt x, CstInt y -> CstBool (x > y)
  | op, _, _ -> failwith ("eval_binop" ^ op) ;;


(* fonction auxiliaire : évaluation d'un opérateur binaire *)
let eval_stackop (stk:stack) (op :string) : stack = 
  match op with
  | "DUP" -> 
      (match stk with
       | [] -> []
       | x::l -> x::x::l)
  | "DROP" ->
      (match stk with
       | [] -> []
       | x::l -> l)
  | "SWAP" ->
      (match stk with
       | [] -> []
       | a::b::l -> b::a::l
       | a::[] -> stk)
  | "ROT" ->
      (match stk with 
       | [] -> []
       | a::b::c::l -> b::c::a::l
       | a::b::[] -> failwith "err ROT 3 min"
       | a::[] -> failwith "err ROT 3 min")
  | _ -> 
      (match stk with 
       | [] -> []
       | x::y::l -> (eval_binop op y x)::l
       | _ -> failwith "eval_stackop") ;;

(* [step stk e] exécute l'élément [e] dans la pile [stk] 
   et retourne la pile résultante *)
let step (stk:stack) (e:element) : stack =
  match e with
  | CstInt _ | CstBool _ -> e::stk
  | OpPile op | OpAri op | OpComp op -> eval_stackop stk op
  | _ -> invalid_arg "step" ;;

(* *********** Question 3 *********** *)

(* Fonction calc execute un programme element par element et renvoie la pile
   à la fin d'execution *)

let rec calc (stk:stack) (p:prog) : stack =
  match p with
  | [] -> stk
  | x::l -> calc (step stk x) l

(* *********** Question 4 *********** *)

(* On represente un dictionnaire sous forme d'un ABR. Cette
   representation est optimale car elle fourni la meilleure
     complexite pire cas pour les operations de recherche et ajout(sauf ajout
   dans la tete) *)

type name = string
type dico = 
  | Empty
  | Node of dico * prog * name * dico

let empty : dico =  Node (Node (Empty, [OpPile "DROP"], "DROP", Empty), [OpPile "DUP"], "DUP",
                          Node
                            (Node (Node (Empty, [CstBool false], "FALSE", Empty), [OpPile "ROT"],
                                   "ROT", Empty),
                             [OpPile "SWAP"], "SWAP", Node (Empty, [CstBool true], "TRUE", Empty)))
;;

(* Les clefs dans notre ABR sont les nommes des listes des mots. 
   Dans la fonction str_compare on defini relation d'ordre sur
   l'ensemble des mots *)

let str_compare (s1 : string) (s2 : string) : int =
  let rec compare_chars (i : int) : int =
    if i >= String.length s1 || i >= String.length s2 then
      compare (String.length s1) (String.length s2)
    else
      let char_a = int_of_char (String.get s1 i) in 
      let char_b = int_of_char (String.get s2 i) in
      if char_a = char_b then compare_chars (i+1)
      else if char_a < char_b then -1
      else 1
  in compare_chars 0 ;;

(* Les fonctions add et lookup permettent de ajouter et chercher
   liste de mots dans dictionnaire respectivement. Si on ajoute
   une liste de mots avec un nom deja present dans dictionnaire 
   la redefinition est effectuee (on cree un nouveau dictionnaire
   avec la nouvelle definition et sans ancienne, alors c'est 
   la nouvelle liste de mots qui sera associee au nom). *)
    
let rec add (x:name) (def:prog) (dico:dico) : dico =
  match dico with
  | Empty -> Node(Empty, def, x, Empty)
  | Node(left, prog, name, right) ->
      let cmp = str_compare x name in
      if cmp = 0 then (*Chaines egales donc rien a modifier*)
        Node(left, def, name, right)
      else if cmp = -1 then (* x < name *)
        Node (add x def left, prog, name, right)
      else (* x > name *)
        Node (left, prog, name, add x def right) ;;

let rec lookup (x:name) (dico:dico) : prog =
  match dico with
  | Empty -> raise Not_found
  | Node (left, prog, name, right) ->
      let compare = str_compare x name in
      if compare = 0 then prog
      else if compare = -1 then lookup x left
      else lookup x right

(* *********** Question 5 *********** *)

(* Grace a fonction evdef on peut traiter les declarations des definitions.
   La fonction aussi verifie la validite de declaration de definition
   et renvoie dictionnaire modifie et le programme apres
   avoir traite tous les elements de definitions *)

let rec evdef (dico:dico) (p:prog) (acc:prog) (cnt:int) (mt : string) : dico * prog = 
  match p with
  | [] -> failwith ("invalid declaration")
  | x::l -> if cnt = 0 then match x with
      | Mot ";" -> failwith ("invalid declaration")
      | Mot ":" -> failwith ("invalid declaration")
      | OpPile s -> failwith ("invalid declaration")
      | OpComp s -> failwith ("invalid declaration")
      | OpAri s -> failwith ("invalid declaration")
      | CstBool b -> failwith ("invalid declaration")
      | CstInt i -> failwith ("invalid declaration")
      | _ -> evdef dico l acc (cnt + 1) (to_string x)
      else if cnt = 1 then match x with 
        | Mot ";" -> failwith ("invalid declaration")
        | Mot ":" -> let nev = evdef dico l [] 0 "" in evdef (fst nev) (snd nev) (acc) (cnt + 1) mt 
        | _ -> evdef dico l (acc @ [x]) (cnt + 1) mt
      else match x with
        | Mot ";" -> (add mt acc dico, l)
        | Mot ":" -> let nev = evdef dico l [] 0 "" in evdef (fst nev) (snd nev) (acc) (cnt + 1) mt
        | _ -> evdef dico l (acc @ [x]) (cnt + 1) mt
                                                             
   (* La fonction evcond permet de traiter les structures de controle
   conditionnelles. On prend en compte le cas des constructions imbriqués *)

let rec evcond (p:prog) (acc : prog) (bl : bool) (idx : int) (cnt : int) : prog * prog =
  if bl = true then 
    match p with
    | [] -> failwith ("invalid condition")
    | x::l -> match x with
      | Mot "IF" -> if idx = 0 then evcond l (acc @ [x]) bl idx (cnt + 1)
          else evcond l acc bl idx (cnt + 1)
      | Mot "THEN" -> if cnt = 0 then acc, l else
          if idx = 0 then evcond l (acc @ [x]) bl idx (cnt - 1)
          else evcond l acc bl idx (cnt - 1)
      | Mot "ELSE" -> if cnt = 0 then evcond l acc bl 1 cnt else
          if idx = 0 then evcond l (acc @ [x]) bl idx cnt
          else evcond l acc bl idx cnt
      | Mot "ENDIF" -> if cnt = 0 then acc, l else
          if idx = 0 then evcond l (acc @ [x]) bl idx (cnt - 1)
          else evcond l acc bl idx (cnt - 1)
      | _ -> if idx = 0 then evcond l (acc @ [x]) bl idx cnt
          else evcond l acc bl idx cnt
  else
    match p with
    | [] -> failwith ("invalid condition")
    | x::l -> match x with
      | Mot "IF" -> if idx = 0 then evcond l (acc @ [x]) bl idx (cnt + 1)
          else evcond l acc bl idx (cnt + 1)
      | Mot "THEN" -> if cnt = 0 then acc, l else
          if idx = 0 then evcond l (acc @ [x]) bl idx (cnt - 1)
          else evcond l acc bl idx (cnt - 1)
      | Mot "ELSE" -> if cnt = 0 then evcond l acc bl 0 cnt else
          if idx = 0 then evcond l (acc @ [x]) bl idx cnt
          else evcond l acc bl idx cnt
      | Mot "ENDIF" -> if cnt = 0 then acc, l else
          if idx = 0 then evcond l (acc @ [x]) bl idx (cnt - 1)
          else evcond l acc bl idx (cnt - 1)
      | _ -> if idx = 0 then evcond l (acc @ [x]) bl idx cnt
          else evcond l acc bl idx cnt
  
   
   (* La fonction eval permet d'executer un programme passé en parametre
   et renvoie la pile apres l'execution de programme. On traite le programme
   element par element. Si element est une constante entiere ou booleenne
     on lui ajoute dans la pile. Si element est un operateur de pile,
     operateur arithmetique ou operatuer de comparaison, on lui evalue.
   Si on a une definition de nouveau mot on lui ajoute dans dictionnaire.
   Si element signifie le debut de structure conditionnelle, on
     traite cette structure. Sinon, on cherche element dans dictionnaire
     et si on trouve, on lui remplace par sa definition et execute, 
   sinon exception est levee *)
  

let rec eval (dico:dico) (stk:stack) (p:prog) : stack = 
  match p with
  | [] -> stk
  | x::l -> try
        eval dico (step stk x) l 
      with
        Invalid_argument _ -> match x with
        | Mot ":" -> let df = evdef dico l [] 0 "" in
            let newdico = fst df in
            let newp = snd df in
            eval newdico stk newp 
        | Mot "IF" ->  (match stk with
            | [] -> invalid_arg "eval"
            | y::ys -> (match y with
                | CstBool b -> if b = true then let cnd = evcond l [] true 0 0 in
                      eval dico (eval dico (step (stk) (OpPile "DROP")) (fst cnd)) (snd cnd)
                    else let cnd = evcond l [] false 1 0 in eval dico (eval dico (step (stk) (OpPile "DROP")) (fst cnd)) (snd cnd)
                | _ -> raise Not_found ))
        | Mot s ->
            let pr = lookup s dico in
            eval dico (eval dico stk pr) l 
        | _ -> raise Not_found
  
   (* Dans notre implementation tous les mots cles sont obliges
   etre en majuscules *)
  
let jeux_de_test = [ (": FACT DUP 1 > IF DUP 1 - FACT * THEN ; 6 FACT", "720") ]

   (* Notre implementation n'est pas parfaite, par exemple, on traite
   les mots declares localement de meme maniere comme ceux declares globalement.
     Cette representation n'est pas tres efficace en termes de memoire, par contre
   si on a declare variable dans un IF on peut lui utiliser meme apres avoir
   quitte cet IF, alors pas besoin de declarer encore. *)

(* *********** Question 6 *********** *)

let carre n = 
  Printf.sprintf ": CARRE DUP * ; %d CARRE" n

let fib n = 
  Printf.sprintf ": FIB DUP 2 < IF ELSE DUP 1 - FIB SWAP 2 - FIB + ENDIF ; %d FIB" n

(* *********** Question 7 *********** *)

let jeux_test = [((fun s -> to_string (of_string s)) "SWAP", "SWAP");
                 ((fun stk -> fun e -> text (step (parse stk) (of_string e))) "2 3 5 1" "DROP", "3 5 1");
                 ((fun stk -> fun e -> text (step (parse stk) (of_string e))) "2 3 5 1" "ROT", "3 5 2 1");
                 ((fun p -> text (calc [] (parse p))) "20 3 *", "60");
                 ((fun p -> text (calc [] (parse p))) "1 2 5 + *", "7");
                 (text (lookup "test" (add ("test") (parse "1 2 3") empty)), "1 2 3");
                 ((fun p -> text (eval empty [] (parse p))) ": double 2 * ; 4 double", "8");
                 ((fun p -> text (eval empty [] (parse p))) "TRUE IF 1 THEN 3", "3 1");
                 ((fun p -> text (eval empty [] (parse p))) ": X : Y 10 2 + ; Y DUP * ; X 2 *", "288");
                 ((fun p -> text (eval empty [] (parse p))) "TRUE IF TRUE IF 5 THEN 6 THEN 7", "7 6 5");
                 ((fun p -> text (eval empty [] (parse p))) "FALSE IF TRUE IF 5 THEN 6 THEN 7", "7");
                 ((fun p -> text (eval empty [] (parse p))) "TRUE IF 4 ELSE 2 ENDIF", "4");
                 ((fun p -> text (eval empty [] (parse p))) "FALSE IF TRUE IF 5 ELSE 8 ENDIF ELSE 9 ENDIF", "9");
                 ((fun p -> text (eval empty [] (parse p))) "TRUE IF TRUE IF 5 ELSE 8 ENDIF THEN 9", "9 5");
                ]
  
let test tests = 
  List.iter (fun ( input, expected_output) -> if input = expected_output then 
                print_endline (input ^ " => " ^ expected_output ^ " CORRECT") else
                print_endline (input ^ " => " ^ expected_output ^ " ERREUR"))
    tests;;

test jeux_test
  
                
  