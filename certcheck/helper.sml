(* Some assorted helper stub functions that's commonly used in the counters *)
fun println s = print (s ^ "\n");

fun fromStringE s =
  case Int.fromString s of
    NONE => raise Fail ("Int.fromString failure: " ^ s)
  | SOME i => i;

fun max x y = if x > y then x else y;

fun max_list n [] = n
| max_list n (x::xs) = max_list (max x n) xs;

fun fst (x,y) = x;

fun snd (x,y) = y;

fun all P [] = true
| all P (x::xs) = P x andalso all P xs;

(* require: n >= 0 *)
fun take n ls =
  if n = 0 then []
  else
    case ls of [] => raise (Fail "list too short")
    | x::xs => x :: take (n-1) xs;

(* require: n >= 0 *)
fun drop n ls =
  if n = 0 then ls
  else
    case ls of [] => raise (Fail "list too short")
    | x::xs => drop (n-1) xs;

fun filter P [] = []
| filter P (x::xs) = if P x then x :: filter P xs else filter P xs;

fun is_space c = (c = #" " orelse c = #"\n" orelse c = #"\t" orelse c = #"\r");

fun inputAllTokens s acc =
  case TextIO.inputLine s of NONE => rev acc
  | SOME st =>
    inputAllTokens s (String.tokens is_space st::acc);

fun print_fmt_file fmt fml file =
  let
    val s = TextIO.openOut file in
    TextIO.output(s,fmt fml);
    TextIO.closeOut s
  end;

val real_to_string = Real.toString;

val real_from_int = Real.fromInt;

(* Get the i-th bit from a byte
  The bits are numbered as follows: 01234567 *)
fun get_bit_from_byte b i =
  Word8.andb(Word8.>>(b,Word.fromInt (7-i)), 0w1:Word8.word) =
  (0w1:Word8.word);

(* Read lS bits starting from position i *)
fun get_bit_from_byte_array ba i =
  let
    val q = i div 8
    val r = i mod 8
  in
    get_bit_from_byte (Word8Vector.sub(ba,q)) r
  end;

(* The range of indexes [i..j) *)
fun range_list i j =
  if i >= j then []
  else i :: range_list (i+1) j;


