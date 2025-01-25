open CertCheck_CNF_EXT;

(* whether to ALLOW more 'flexible' DIMACS CNF formatting *)
val bad_dimacs = false;

(* REQUIREMENT: Implement parser for the theory
  The following is a parser for CNF-XOR-BNN *)

(* Types in concrete DIMACS *)
type fml = lit list list * (lit list list * (lit list * (Arith.nat * lit)) list);
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

fun parse_lit s =
  let
    val n = fromStringE s
    val an = abs n
    val ann = Arith.nat_of_integer an
  in
    if n <= 0 then Neg ann else Pos ann
  end;

fun parse_lits_aux [] acc = raise Fail ("Line not terminated with 0")
| parse_lits_aux (s::ss) acc =
    if s = "0" then (rev acc, ss)
    else
      parse_lits_aux ss (parse_lit s::acc);

fun var_lit (Pos n) = n
| var_lit (Neg n) = n;

fun lit_to_string (Pos n) = Int.toString (Arith.integer_of_nat n)
| lit_to_string (Neg n) = "-"^Int.toString (Arith.integer_of_nat n);

fun check_lit v [] = ()
| check_lit v (l::ls) =
  if Arith.integer_of_nat (var_lit l) <= v then check_lit v ls
  else raise Fail ("Invalid literal: "^lit_to_string l);

fun parse_lits v ss =
  case parse_lits_aux ss [] of
    (ls,[]) => (check_lit v ls; ls)
  | _ => raise Fail ("Stray characters in clause/XOR line");

fun parse_tail v [kstr,ystr,"0"] =
  let
    val k = fromStringE kstr
    val kn = Arith.nat_of_integer k
    val y = parse_lit ystr
  in
    if k <= 0 then
      raise Fail ("Invalid cutoff: " ^ kstr)
    else if Arith.integer_of_nat (var_lit y) <= v then
      (kn, y)
    else
      raise Fail ("Invalid RHS literal: " ^ ystr)
  end
| parse_tail _ _ =
      raise Fail ("Unable to parse end of BNN line");

fun parse_bnn v ss =
  case parse_lits_aux ss [] of
    (ls,res) => (check_lit v ls; (ls, parse_tail v res));

fun parse_rest v [] (cacc,xacc,bacc) = ((rev cacc, (rev xacc, rev bacc)):fml)
| parse_rest v (s::ss) (cacc,xacc,bacc) =
  case s of
    "x"::xs => parse_rest v ss (cacc,parse_lits v xs::xacc,bacc)
  | "b"::xs => parse_rest v ss (cacc,xacc,parse_bnn v xs::bacc)
  | xs => parse_rest v ss (parse_lits v xs::cacc,xacc,bacc);

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
      val fml = parse_rest v rest ([],[],[])
      val proj = conv_proj v projvs
    in
      if
        c = (length (#1 fml)) + (length (#1 (#2 fml))) + (length (#2 (#2 fml)))
        orelse bad_dimacs
      then
        (fml,proj)
      else raise Fail "Incorrect no. of clauses+xors+bnns"
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
