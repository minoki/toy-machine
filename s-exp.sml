structure SExp = struct
datatype exp = ID of string
             | INT of int
             | STRING of string
             | BOOL of bool
             | LIST of exp list
fun toString (ID x) = x
  | toString (STRING s) = "\"" ^ String.toString s ^ "\""
  | toString (INT n) = if n < 0 then
                           "-" ^ String.extract (Int.toString n, 1, NONE)
                       else
                           Int.toString n
  | toString (BOOL false) = "#f"
  | toString (BOOL true) = "#t"
  | toString (LIST xs) = "(" ^ String.concatWith " " (List.map toString xs) ^ ")"
end;
