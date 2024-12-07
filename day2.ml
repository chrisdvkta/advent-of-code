let file = "input.txt"

let read_file_as_arrays filename = 
  let channel = open_in filename in 
  let rec read_lines data = 
    try
      let line = input_line channel in 
      let numbers = Array.of_list(List.map int_of_string (String.split_on_char ' ' line  )) in 
      read_lines (numbers::data)
    with 
    | End_of_file -> 
      close_in channel;
      List.rev data
  in
  read_lines []

  
let is_safe_array arr = 
  let n = Array.length arr in 
  if n<2 then true 
  else 
    let rec check_order i direction = 
      if i = n then true 
      else 
        let diff = arr.(i) - arr.(i-1) in 
        if diff = 0|| abs diff>3 || (direction = 1 && diff<0) || (direction= -1 && diff >0) then 
          false 
      else 
        check_order (i+1) direction
    in
    let initial_diff = arr.(1) - arr.(0) in 
    if abs initial_diff >3 || initial_diff=0 then false 
    else check_order 2 (if initial_diff > 0 then 1 else -1)
    
    


let is_safe_with_removal arr = 
  if is_safe_array arr then true 
  else 
    let n = Array.length arr in 
    let rec try_removal i =
      if i = n then false 
      else 
        let new_arr = Array.init(n-1) (fun j-> if j<i then arr.(j) else arr.(j+1)) in
        if is_safe_array new_arr then true else try_removal (i+1)
    in
    try_removal 0





let count_safe_arrays arrays = 
  List.fold_left 
  (fun count arr -> if is_safe_with_removal arr then count+1 else count)
  0 
  arrays

  
let () = 
  let data = read_file_as_arrays file in 
  let valid_count =count_safe_arrays data in 
  Printf.printf "number of valid arrays : %d\n" valid_count
   