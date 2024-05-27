(* REQUIREMENT: Implement printer for the theory
  The following is a printer for CNF-XOR which will be
  used to call the external UNSAT checker *)
fun lit_to_string (Pos n) = Int.toString (Arith.integer_of_nat n)
| lit_to_string (Neg n) = "-"^Int.toString (Arith.integer_of_nat n);

fun clause_to_string cs =
  String.concatWith " " (map lit_to_string cs @ ["0"]);

fun xor_to_string cs =
  "x "^clause_to_string cs;

fun lit_var (Pos n) = Arith.integer_of_nat n
| lit_var (Neg n) = Arith.integer_of_nat n;

fun max_var_clause cs = max_list 0 (map lit_var cs);

fun max_var_fml (cs,xs) =
  max
  (max_list 0 (map max_var_clause cs))
  (max_list 0 (map max_var_clause xs));

fun header_string v c =
  "p cnf "^Int.toString v^" "^Int.toString c;

fun fml_to_string (cs,xs) =
  let
    val h = header_string (max_var_fml (cs,xs)) (length cs + length xs)
  in
    String.concatWith "\n"
      (h::map clause_to_string cs @ map xor_to_string xs)
  end;

(* Get the nth XOR from the byte array *)
fun get_xor_from_byte_array lSI n ba =
  (List.map (get_bit_from_byte_array ba)
    (range_list (n * (lSI+1)) (n * (lSI+1) + lSI)),
  get_bit_from_byte_array ba (n * (lSI+1) + lSI));

fun gen_rand_xors t lS ba =
  let
    val tI = Arith.integer_of_nat t
    val lSI = Arith.integer_of_nat lS
    val xors = lSI + 1
    val width = if lSI = 0 then 0 else lSI - 1
    val nbits = tI * xors * width
  in
    if nbits <= Word8Vector.length ba * 8
    then
      (fn i => fn j =>
      let
        val iI = Arith.integer_of_nat i;
        val jI = Arith.integer_of_nat j
      in
        if iI < tI andalso jI < width
        then
          get_xor_from_byte_array lSI (iI * width + jI) ba
        else
          raise Fail "randomness out of bounds"
      end)
    else
      raise Fail "not enough randomness"
  end;

(* Helper function to print bit strings as XORs *)
fun xorb_to_string_aux [] [] = []
| xorb_to_string_aux (x::xs) (b::bs) =
  if b then Int.toString x :: xorb_to_string_aux xs bs
  else xorb_to_string_aux xs bs
| xorb_to_string_aux _ _ = raise Fail "mismatched bits";

fun xorb_to_string S (bs,b) =
  (String.concatWith "+" (xorb_to_string_aux S bs) ^ " = " ^
    (if b then "1" else "0"));

(* Print all t * (lS-1) XORs for debug purposes *)
fun print_rand_xors S t lS xors =
  let
    val tI = Arith.integer_of_nat t
    val lSI = Arith.integer_of_nat lS
    val width = if lSI = 0 then 0 else lSI - 1
  in
    app
    (fn iI =>
    app
    (fn jI =>
      (print ("c round " ^ (Int.toString iI) ^ " xor "
        ^ (Int.toString jI) ^ ": ");
      println
      (xorb_to_string S
      (xors (Arith.nat_of_integer iI) (Arith.nat_of_integer jI)))))
    (range_list 0 width)
    )
    (range_list 0 tI)
  end;

(* Solution output should be of this form:
 num_sols
 ... sol line ...
 ... *)
fun parse_sol [0] = []
| parse_sol (n::ss) = (
  let
    val an = abs n
    val l = n >= 0 in
    if an = 0 then raise Fail ("invalid assg: "^ (Int.toString n))
    else (an,l) :: parse_sol ss
  end)
| parse_sol _ = raise Fail "line not terminated with 0";

fun fill_arr arr [] = ()
| fill_arr arr ((an,l)::xs) =
  (Array.update(arr,an,l); fill_arr arr xs);

fun sol_fun ls =
  let
    val mv = max_list 0 (map fst ls) + 1
    val arr = Array.array(mv,false)
    val u = fill_arr arr ls
  in
    (fn n =>
    let
      val nn = Arith.integer_of_nat n in
      if nn < mv
      then
        Array.sub(arr,nn)
      else false
    end)
  end;

fun inputLine s =
  case TextIO.inputLine s of NONE => NONE
  | SOME st =>
    SOME (map fromStringE (String.tokens is_space st));

fun inputNLines s n acc =
  if n = 0 then rev acc
  else
    case inputLine s of NONE =>
      raise Fail "insufficient lines left"
    | SOME st =>
      inputNLines s (n-1) (st::acc);

(* read a block of solutions *)
fun parse_sols s =
  case inputLine s of
    SOME [h] =>
    if h >= 0 then
      (map (sol_fun o parse_sol) (inputNLines s h []))
    else raise Fail "negative solution count"
  |  _ => raise Fail "unable to parse solutions";

fun parse_m0 s =
  (case inputLine s of
    SOME [0] => parse_sols s
  | _ => raise Fail "fail to parse round 0")

(* parse all remaining rounds *)
fun parse_ms s =
  case inputLine s of
    SOME [m] =>
    let
      val c1 = parse_sols s
      val c2 = parse_sols s
    in
      (Arith.nat_of_integer m,(c1,c2)) :: parse_ms s
    end
  | NONE => []
  | _ => raise Fail "fail to parse round" ;

fun parse_ms_file ms_file =
  let
    val s = TextIO.openIn ms_file
    val m0 = parse_m0 s
    val ls = parse_ms s
  in
    (m0, fn n => List.nth ls n)
  end;

(* OPTIONAL: configurable specifics for the rest of the setup *)

(* The temporary file path *)
val temp_path_suffix = ref "approxmc_temp.xnf";

(* There should be exactly one line of output *)
fun check_lines lines =
  if lines = [["SUCCESS"]]
  then true
  else
    (println ("c expect 'SUCCESS' but got: " ^
    (String.concatWith "\n"
      (map (String.concatWith " ") lines))); false);

(* REQUIREMENT: Implement an UNSAT proof checking oracle *)
fun check_unsat fname cmd_path F =
  (let
    val name = fname ^"_"^ !temp_path_suffix
    val u = print_fmt_file fml_to_string F name
    val args = [name]
    val _ = println ("c dumping CNF-XOR to file: "^ name)
    val _ = println ("c and calling UNSAT checker: "^ cmd_path)
    val proc : (TextIO.instream, TextIO.outstream) Unix.proc =
      Unix.execute (cmd_path, args);
    val ins = Unix.textInstreamOf proc;
    val lines = inputAllTokens ins [] in
    TextIO.closeIn ins;
    OS.FileSys.remove name;
    Unix.reap proc;
    println ("c shutdown UNSAT checker");
    check_lines lines
  end) handle
    Fail s => raise Fail s
  | _ => raise Fail "External UNSAT checker call failed";

(* Other helper functions *)
fun eq_int (x:int) y = (x = y);
fun less_eq_int (x:int) y = (x <= y);
fun less_int (x:int) y = (x < y);

val equal_int = {equal = eq_int} : int HOL.equal;

val ord_int = {less_eq = less_eq_int, less = less_int} : int Orderings.ord;

val preorder_int = {ord_preorder = ord_int} : int Orderings.preorder;

val order_int = {preorder_order = preorder_int} : int Orderings.order;

val linorder_int = {order_linorder = order_int} : int Orderings.linorder;

fun approxmc F S eps del cert xors fname cuname blast =
  if blast then
    certcheck_blast (fn f => (check_unsat fname cuname (f,[])))
      F S eps del cert xors
  else
    certcheck (check_unsat fname cuname)
      F S eps del cert xors;

val usage = "usage: certcheck_cnf_xor eps del foo.xnf rand_file cert_file check_unsat_path [optional: blast (blast to CNF)]";

fun parse_blast [s] = (s = "blast")
| parse_blast _ = false;

fun parse_args (streps::strdel::fname::rname::mname::cuname::rest) =
  (let
    val eps = real_from_string streps
    val del = real_from_string strdel
    val (F,S) = (parse_fmlp_file fname)
    val t = CertCheck_CNF_XOR.find_t del;
    val lS = List.size_list S;
    val blast = parse_blast rest;
    val _ = println
      ("c using eps: " ^ rat_real_to_string eps)
    val _ = println
      ("c using del: " ^ rat_real_to_string del)
    (* val _ = println
      ("c projection set: " ^ (
      String.concatWith " "
        (xorb_to_string_aux S
        (map (fn n => true) (range_list 0 (length S)))))); *)
    val _ = println
      ("c projection set length: "^(Int.toString o Arith.integer_of_nat) lS);
    val _ = println
      ("c iters: "^(Int.toString o Arith.integer_of_nat) t);
    val _ = println
      ("c thresh: "^(Int.toString o Arith.integer_of_nat) (ApproxMCAnalysis.compute_thresh eps));
    val rand = BinIO.inputAll (BinIO.openIn rname);
    val _ = println ("c read rand bits: "^Int.toString (Word8Vector.length rand * 8))
    val _ = println ("c using UNSAT checker: "^ cuname)
    val _ = println ("c blast to CNF: " ^ (if blast then "true" else "false"))
    val xors = gen_rand_xors t lS rand
    (* val _ = print_rand_xors S t lS xors *)

    val cert = parse_ms_file mname

    val cnte = approxmc F S eps del cert xors fname cuname blast
  in
    case cnte of
      Sum_Type.Inl err => println ("c CERT ERROR: " ^ err)
    | Sum_Type.Inr cnt =>
    println ("s mc "^
      (Int.toString o Arith.integer_of_nat) cnt)
  end handle Fail s => println ("c ERROR: "^s))
| parse_args _ =
  println"usage: certcheck_cnf_xor eps del foo.xnf rand_file cert_file check_unsat_path [optional: blast=T/F]"

val args = CommandLine.arguments ();
val u = parse_args args;
