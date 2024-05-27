open CertCheck_CNF_XOR;

(* whether to ALLOW more 'flexible' DIMACS CNF formatting *)
val bad_dimacs = true;

(* REQUIREMENT: Implement parser for the theory
  The following is a parser for CNF-XOR *)

(* Types in concrete DIMACS *)
type fml = lit list list * lit list list;
type proj = Arith.nat list;

(* header line *)
fun parse_header s =
  case s of
    ["p","cnf",v,c] => (fromStringE v,fromStringE c)
  | _ => raise Fail ("Failed to parse DIMACS header");

(* variables line *)
fun parse_vars ["0"] = []
| parse_vars (s::ss) = (
  let
    val n = fromStringE s in
    if n <= 0 then
      raise Fail ("Invalid var: " ^ s)
    else n :: parse_vars ss
  end)
| parse_vars _ = raise Fail "Line not terminated with 0";

fun parse_projs (s::ss) accl accp =
  (case s of
    "c"::"ind"::rest =>
      parse_projs ss accl (parse_vars rest::accp)
  | _ =>
      parse_projs ss (s::accl) accp)
| parse_projs [] accl accp = (rev accl, rev accp);

fun parse_clause v ["0"] = []
| parse_clause v (s::ss) = (
  let
    val n = fromStringE s
    val an = abs n
    val ann = Arith.nat_of_integer an
    val l = if n <= 0 then Neg ann else Pos ann in
    if an = 0 orelse an > v then raise Fail ("Invalid lit: "^ s)
    else l :: parse_clause v ss
  end)
| parse_clause v _ = raise Fail "Line not terminated with 0";

fun parse_rest v [] (cacc,xacc) = (rev cacc, rev xacc)
| parse_rest v (s::ss) (cacc,xacc) =
  case s of
    "x"::xs => parse_rest v ss (cacc,parse_clause v xs::xacc)
  | xs => parse_rest v ss (parse_clause v xs::cacc,xacc);

(* If no c ind lines are present, assume we are
  counting the entire formula *)
fun mk_proj v acc =
  if v > 0
  then mk_proj (v-1) (Arith.nat_of_integer v::acc)
  else acc;

fun conv_proj v [] = mk_proj v []
| conv_proj v [x] =
  if all (fn n => n > 0 andalso n <= v) x then (map Arith.nat_of_integer x)
  else
    raise Fail ("Invalid var in projection set")
| conv_proj v (x::xs) =
  conv_proj v [x] @ conv_proj v xs;

fun parse_fmlp_aux lines =
  let
    val (lines, projvs) = parse_projs lines [] []
  in
    case lines of [] => raise Fail "Missing DIMACS header"
    | (hdline :: rest) =>
    let
      val (v,c) = parse_header hdline
      val fml = parse_rest v rest ([],[])
      val proj = conv_proj v projvs
    in
      if
        c = (length (fst fml)) + (length (snd fml))
        orelse bad_dimacs
      then
        (fml,proj)
      else raise Fail "Incorrect no. of clauses"
    end
  end

fun is_comment s =
  case s of
    "c":: x :: rest => not(x = "ind")
  | ["c"] => true
  | [] => bad_dimacs
  | _ => false;

fun filter_comments ls =
  filter (fn l => not(is_comment l)) ls;

val parse_fmlp = parse_fmlp_aux o filter_comments;

fun parse_fmlp_file file : (fml * proj) =
  let
    val s = TextIO.openIn file in
    parse_fmlp (inputAllTokens s [])
  end;

fun real_from_string s =
  case String.tokens (fn c => c = #"/") s of
    [num,dec] =>
    real_div
      (real_of_int (fromStringE num))
      (real_of_int (fromStringE dec))
  | [num] =>
    real_div
      (real_of_int (fromStringE num))
      (real_of_int 1)
  | _ => raise Fail ("rat_from_string failure: " ^ s);

fun rat_real_to_string (Real.Ratreal r) =
  let val (n,d) = Rat.quotient_of r in
    (Int.toString o Arith.integer_of_int) n ^ "/" ^
    (Int.toString o Arith.integer_of_int) d
  end;
