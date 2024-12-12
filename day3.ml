(* 
1. brute force approach 
  find m, as soon as m is scanned, scan for the next set of characters ie ul(
  if ^ succeeds then scan a number store it to x 
  scan for , 
  scan for another number, store it to y 
  scan for )
  success. 
  add it to sum +=result

2.optimal way
  i got nothing. 
*)



let opening = "mul("
let closing = ")"
let do_ins = "do()"
let dont_ins = "don't()"

let read_file_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let index_sub str sub pos =
  let sub_len = String.length sub in
  let str_len = String.length str in
  let rec search i =
    if i + sub_len > str_len then raise Not_found
    else if String.sub str i sub_len = sub then i
    else search (i+1)
  in
  search pos

let index_of str sub =
  try Some (index_sub str sub 0)
  with Not_found -> None

let remove_substring str pos len =
  let before = String.sub str 0 pos in
  let after = String.sub str (pos+len) (String.length str - pos - len) in
  before ^ after


let eval_mul_expression expr =
  let open_len = String.length opening in
  let close_len = String.length closing in
  let expr_len = String.length expr in
  if not (String.length expr >= open_len + close_len &&
          String.sub expr 0 open_len = opening &&
          String.sub expr (expr_len - close_len) close_len = closing)
  then None
  else
    let inside = String.sub expr open_len (expr_len - open_len - close_len) in
    match String.split_on_char ',' inside with
    | [arg1; arg2] ->
       (try
          let x = int_of_string (String.trim arg1) in
          let y = int_of_string (String.trim arg2) in
          Some (x * y)
        with Failure _ -> None)
    | _ -> None


let find_next_instruction line =
  let candidates = [
    (index_of line do_ins, `Do, String.length do_ins);
    (index_of line dont_ins, `Dont, String.length dont_ins);
    (index_of line opening, `MulStart, String.length opening)
  ] in
  let candidates = List.fold_left (fun acc (pos_opt, kind, length) ->
      match pos_opt with
      | Some p -> (p, kind, length) :: acc
      | None -> acc
    ) [] candidates
  in
  if candidates = [] then None
  else
     let sorted = List.sort (fun (p1,_,_) (p2,_,_) -> compare p1 p2) candidates in
    Some (List.hd sorted)

let process_line line enabled_ref score_ref =
  let rec loop l =
    match find_next_instruction l with
    | None -> l
    | Some (pos, kind, length) -> 
      begin
        match kind with
        | `Do ->
          enabled_ref := true;
          let l = remove_substring l pos length in
          loop l
        | `Dont ->
          enabled_ref := false;
          let l = remove_substring l pos length in
          loop l
        | `MulStart ->
          (match index_of l closing with
           | None ->
             let l = remove_substring l pos (String.length opening) in
             loop l
           | Some c_pos ->
             if c_pos < pos then
               let l = remove_substring l c_pos 1 in
               loop l
             else
               let dist = abs ((pos+4) - c_pos) in
               if dist > 7 then
                 let l = remove_substring l pos (String.length opening) in
                 loop l
               else
                 let expr_len = (c_pos - pos) + 1 in
                 let expr = String.sub l pos expr_len in
                 (match eval_mul_expression expr with
                  | Some result ->
                     if !enabled_ref then score_ref := !score_ref + result
                  | None ->
                     Printf.eprintf "Not valid function: %s\n" expr);
                 let l = remove_substring l pos expr_len in
                 loop l
          )
      end
  in
  ignore (loop line)

let () =
  let data = read_file_lines "input.txt" in
  let score = ref 0 in
  let enabled = ref true in  
  List.iter (fun line ->
      process_line line enabled score
    ) data;
  Printf.printf "%d\n" !score
