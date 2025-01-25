(* Default *)
val gen = ref (Random.newgenseed 42.0);

(* 0 to 255 inclusive *)
fun random_byte u =
  Word8.fromInt (Random.range (0,256) (!gen));

fun bit_to_string true = "1"
| bit_to_string false = "0";

fun print_byte b = (
  print (bit_to_string (get_bit_from_byte b 7));
  print (bit_to_string (get_bit_from_byte b 6));
  print (bit_to_string (get_bit_from_byte b 5));
  print (bit_to_string (get_bit_from_byte b 4));
  print (bit_to_string (get_bit_from_byte b 3));
  print (bit_to_string (get_bit_from_byte b 2));
  print (bit_to_string (get_bit_from_byte b 1));
  print (bit_to_string (get_bit_from_byte b 0)));

(* Generates byte by byte *)
fun print_random_bits n rfile =
  if n <= 0 then ()
  else
    let val b = random_byte () in
      (* print_byte b; *)
      BinIO.output1(rfile,b);
      print_random_bits (n-8) rfile
    end;

(* Dump t * (lS - 1) * (lS + 1) random bits *)
fun gen_rand_raw tI lSI rfile =
  let
    val xors = lSI + 1
    val width = if lSI = 0 then 0 else lSI - 1
    val nbits = tI * xors * width
  in
    println("c Generating random bits: " ^
      Int.toString nbits);
    print_random_bits nbits rfile
  end;

fun gen_rand t lS rfile =
  gen_rand_raw
    (Arith.integer_of_nat t) (Arith.integer_of_nat lS)
    rfile;

(* TODO: hash the file to generate more deterministic randomness *)
fun parse_args [streps,strdel,fname,rname] =
  (let
    val eps = real_from_string streps
    val del = real_from_string strdel
    val (F,S) = (parse_fmlp_file fname)
    val u = (gen := Random.newgen())
    val t = CertCheck_CNF_EXT.find_t del;
    val lS = List.size_list S;

    val _ = println
      ("c using eps: " ^ rat_real_to_string eps)
    val _ = println
      ("c using del: " ^ rat_real_to_string del)
    val _ = println
      ("c projection set length: "^(Int.toString o Arith.integer_of_nat) lS);
    val _ = println
      ("c iters: "^(Int.toString o Arith.integer_of_nat) t);

    val rfile = BinIO.openOut rname
  in
    gen_rand t lS rfile
  end handle Fail s => println ("c ERROR: "^s))
| parse_args _ =
  println"usage: gen_rand eps del foo.xnf rand_file"

val args = CommandLine.arguments ();
val u = parse_args args;
