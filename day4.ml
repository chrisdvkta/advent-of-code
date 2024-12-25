(* 
test input -> 
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX

Opposite of divine intellect algo:  
Log everytime you find X, check neighbor nodes for M (recursion 1), log direction of M, call a discovery function in the direction from M
end when pattern does not match. 
result+=1 when it matches pattern. 
Continue the parse. 


divine intellect algo: I got nothing. 

Powered by Terry Davis. 
*)


let file = "input.txt"

let read_file_lines filename = 
  let ic = open_in filename in 
  try 
    let line = input_line ic in 
    print_endline line; 
    flush stdout; 
    close_in ic
with e-> 
  close_in_noerr ic; 
  raise e



let () = read_file_lines file;;
