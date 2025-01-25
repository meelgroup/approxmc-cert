structure Str_Literal =
struct

fun map f [] = []
  | map f (x :: xs) = f x :: map f xs; (* deliberate clone not relying on List._ module *)

fun check_ascii (k : IntInf.int) =
  if 0 <= k andalso k < 128
  then k
  else raise Fail "Non-ASCII character in literal";

val char_of_ascii = Char.chr o IntInf.toInt o check_ascii;

val ascii_of_char = check_ascii o IntInf.fromInt o Char.ord;

val literal_of_asciis = String.implode o map char_of_ascii;

val asciis_of_literal = map ascii_of_char o String.explode;

end;

structure Fun : sig
  val id : 'a -> 'a
end = struct

fun id x = (fn xa => xa) x;

end; (*struct Fun*)

structure HOL : sig
  type 'a equal
  val equal : 'a equal -> 'a -> 'a -> bool
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};
val equal = #equal : 'a equal -> 'a -> 'a -> bool;

fun eq A_ a b = equal A_ a b;

end; (*struct HOL*)

structure Product_Type : sig
  val equal_boola : bool -> bool -> bool
  val equal_bool : bool HOL.equal
  val apsnd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  val map_prod : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val equal_prod : 'a HOL.equal -> 'b HOL.equal -> 'a * 'b -> 'a * 'b -> bool
end = struct

fun equal_boola p true = p
  | equal_boola p false = not p
  | equal_boola true p = p
  | equal_boola false p = not p;

val equal_bool = {equal = equal_boola} : bool HOL.equal;

fun apsnd f (x, y) = (x, f y);

fun map_prod f g (a, b) = (f a, g b);

fun fst (x1, x2) = x1;

fun snd (x1, x2) = x2;

fun equal_prod A_ B_ (x1, x2) (y1, y2) =
  HOL.eq A_ x1 y1 andalso HOL.eq B_ x2 y2;

end; (*struct Product_Type*)

structure Orderings : sig
  type 'a ord
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  type 'a preorder
  val ord_preorder : 'a preorder -> 'a ord
  type 'a order
  val preorder_order : 'a order -> 'a preorder
  type 'a no_bot
  type 'a no_top
  type 'a linorder
  val order_linorder : 'a linorder -> 'a order
  type 'a dense_order
  type 'a dense_linorder
  type 'a unbounded_dense_linorder
  val max : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool, less : 'a -> 'a -> bool};
val less_eq = #less_eq : 'a ord -> 'a -> 'a -> bool;
val less = #less : 'a ord -> 'a -> 'a -> bool;

type 'a preorder = {ord_preorder : 'a ord};
val ord_preorder = #ord_preorder : 'a preorder -> 'a ord;

type 'a order = {preorder_order : 'a preorder};
val preorder_order = #preorder_order : 'a order -> 'a preorder;

type 'a no_bot = {order_no_bot : 'a order};
val order_no_bot = #order_no_bot : 'a no_bot -> 'a order;

type 'a no_top = {order_no_top : 'a order};
val order_no_top = #order_no_top : 'a no_top -> 'a order;

type 'a linorder = {order_linorder : 'a order};
val order_linorder = #order_linorder : 'a linorder -> 'a order;

type 'a dense_order = {order_dense_order : 'a order};
val order_dense_order = #order_dense_order : 'a dense_order -> 'a order;

type 'a dense_linorder =
  {dense_order_dense_linorder : 'a dense_order,
    linorder_dense_linorder : 'a linorder};
val dense_order_dense_linorder = #dense_order_dense_linorder :
  'a dense_linorder -> 'a dense_order;
val linorder_dense_linorder = #linorder_dense_linorder :
  'a dense_linorder -> 'a linorder;

type 'a unbounded_dense_linorder =
  {dense_linorder_unbounded_dense_linorder : 'a dense_linorder,
    no_bot_unbounded_dense_linorder : 'a no_bot,
    no_top_unbounded_dense_linorder : 'a no_top};
val dense_linorder_unbounded_dense_linorder =
  #dense_linorder_unbounded_dense_linorder :
  'a unbounded_dense_linorder -> 'a dense_linorder;
val no_bot_unbounded_dense_linorder = #no_bot_unbounded_dense_linorder :
  'a unbounded_dense_linorder -> 'a no_bot;
val no_top_unbounded_dense_linorder = #no_top_unbounded_dense_linorder :
  'a unbounded_dense_linorder -> 'a no_top;

fun max A_ a b = (if less_eq A_ a b then b else a);

end; (*struct Orderings*)

structure Arith : sig
  datatype int = Int_of_integer of IntInf.int
  val integer_of_int : int -> IntInf.int
  val equal_inta : int -> int -> bool
  val equal_int : int HOL.equal
  type nat
  val integer_of_nat : nat -> IntInf.int
  val equal_nata : nat -> nat -> bool
  val equal_nat : nat HOL.equal
  val times_nata : nat -> nat -> nat
  type 'a times
  val times : 'a times -> 'a -> 'a -> 'a
  type 'a dvd
  datatype num = One | Bit0 of num | Bit1 of num
  val one_nata : nat
  type 'a one
  val one : 'a one -> 'a
  val plus_nata : nat -> nat -> nat
  type 'a plus
  val plus : 'a plus -> 'a -> 'a -> 'a
  val zero_nata : nat
  type 'a zero
  val zero : 'a zero -> 'a
  type 'a semigroup_add
  val plus_semigroup_add : 'a semigroup_add -> 'a plus
  type 'a numeral
  type 'a power
  val power_nat : nat power
  val minus_nata : nat -> nat -> nat
  type 'a minus
  val minus : 'a minus -> 'a -> 'a -> 'a
  val divide_nata : nat -> nat -> nat
  type 'a divide
  val divide : 'a divide -> 'a -> 'a -> 'a
  val modulo_integer : IntInf.int -> IntInf.int -> IntInf.int
  val modulo_nata : nat -> nat -> nat
  val less_eq_nat : nat -> nat -> bool
  val less_nat : nat -> nat -> bool
  type 'a ab_semigroup_add
  type 'a monoid_add
  val semigroup_add_monoid_add : 'a monoid_add -> 'a semigroup_add
  val zero_monoid_add : 'a monoid_add -> 'a zero
  type 'a comm_monoid_add
  type 'a mult_zero
  type 'a semigroup_mult
  type 'a semiring
  type 'a semiring_0
  type 'a semiring_no_zero_divisors
  type 'a monoid_mult
  type 'a semiring_numeral
  type 'a zero_neq_one
  type 'a semiring_1
  val semiring_numeral_semiring_1 : 'a semiring_1 -> 'a semiring_numeral
  val semiring_0_semiring_1 : 'a semiring_1 -> 'a semiring_0
  val zero_neq_one_semiring_1 : 'a semiring_1 -> 'a zero_neq_one
  type 'a semiring_1_no_zero_divisors
  type 'a cancel_semigroup_add
  type 'a cancel_ab_semigroup_add
  type 'a cancel_comm_monoid_add
  type 'a semiring_0_cancel
  type 'a ab_semigroup_mult
  type 'a comm_semiring
  type 'a comm_semiring_0
  type 'a comm_semiring_0_cancel
  type 'a semiring_1_cancel
  type 'a comm_monoid_mult
  type 'a comm_semiring_1
  type 'a comm_semiring_1_cancel
  type 'a semidom
  val monoid_add_nat : nat monoid_add
  val zero_neq_one_nat : nat zero_neq_one
  val linorder_nat : nat Orderings.linorder
  type 'a semiring_char_0
  val semiring_1_semiring_char_0 : 'a semiring_char_0 -> 'a semiring_1
  val semiring_char_0_nat : nat semiring_char_0
  type 'a divide_trivial
  type 'a semiring_no_zero_divisors_cancel
  type 'a semidom_divide
  type 'a semiring_modulo_trivial
  type 'a algebraic_semidom
  type 'a semidom_modulo
  val semidom_modulo_nat : nat semidom_modulo
  val zero_neq_one_integer : IntInf.int zero_neq_one
  type 'a comm_semiring_1_cancel_crossproduct
  type 'a uminus
  val uminus : 'a uminus -> 'a -> 'a
  type 'a group_add
  type 'a ab_group_add
  type 'a ring
  type 'a ring_no_zero_divisors
  type 'a neg_numeral
  type 'a ring_1
  type 'a ring_1_no_zero_divisors
  type 'a comm_ring
  type 'a comm_ring_1
  type 'a idom
  type 'a idom_divide
  type 'a inverse
  val divide_inverse : 'a inverse -> 'a divide
  val inverse : 'a inverse -> 'a -> 'a
  type 'a division_ring
  type 'a field
  type 'a abs
  val abs : 'a abs -> 'a -> 'a
  type 'a abs_if
  val abs_abs_if : 'a abs_if -> 'a abs
  val minus_abs_if : 'a abs_if -> 'a minus
  val uminus_abs_if : 'a abs_if -> 'a uminus
  val zero_abs_if : 'a abs_if -> 'a zero
  val ord_abs_if : 'a abs_if -> 'a Orderings.ord
  type 'a ring_char_0
  type 'a sgn
  val sgn : 'a sgn -> 'a -> 'a
  type 'a idom_abs_sgn
  type 'a ordered_ab_semigroup_add
  type 'a ordered_comm_monoid_add
  type 'a ordered_semiring
  type 'a ordered_semiring_0
  type 'a ordered_cancel_semiring
  type 'a strict_ordered_ab_semigroup_add
  type 'a ordered_cancel_ab_semigroup_add
  type 'a ordered_ab_semigroup_add_imp_le
  type 'a strict_ordered_comm_monoid_add
  type 'a ordered_cancel_comm_monoid_add
  type 'a ordered_ab_semigroup_monoid_add_imp_le
  type 'a ordered_ab_group_add
  type 'a ordered_ring
  type 'a field_char_0
  type 'a zero_less_one
  type 'a field_abs_sgn
  type 'a linordered_ab_semigroup_add
  type 'a linordered_cancel_ab_semigroup_add
  type 'a linordered_semiring
  type 'a linordered_semiring_strict
  type 'a linordered_semiring_1
  type 'a linordered_semiring_1_strict
  type 'a ordered_ab_group_add_abs
  type 'a linordered_ab_group_add
  type 'a linordered_ring
  val linordered_ab_group_add_linordered_ring :
    'a linordered_ring -> 'a linordered_ab_group_add
  val ordered_ab_group_add_abs_linordered_ring :
    'a linordered_ring -> 'a ordered_ab_group_add_abs
  val abs_if_linordered_ring : 'a linordered_ring -> 'a abs_if
  val linordered_semiring_linordered_ring :
    'a linordered_ring -> 'a linordered_semiring
  val ordered_ring_linordered_ring : 'a linordered_ring -> 'a ordered_ring
  type 'a linordered_ring_strict
  val linordered_ring_linordered_ring_strict :
    'a linordered_ring_strict -> 'a linordered_ring
  val linordered_semiring_strict_linordered_ring_strict :
    'a linordered_ring_strict -> 'a linordered_semiring_strict
  val ring_no_zero_divisors_linordered_ring_strict :
    'a linordered_ring_strict -> 'a ring_no_zero_divisors
  type 'a ordered_comm_semiring
  type 'a ordered_cancel_comm_semiring
  type 'a linordered_comm_semiring_strict
  type 'a linordered_nonzero_semiring
  type 'a linordered_semidom
  type 'a ordered_comm_ring
  type 'a ordered_ring_abs
  type 'a linordered_idom
  val ring_char_0_linordered_idom : 'a linordered_idom -> 'a ring_char_0
  val idom_abs_sgn_linordered_idom : 'a linordered_idom -> 'a idom_abs_sgn
  val linordered_ring_strict_linordered_idom :
    'a linordered_idom -> 'a linordered_ring_strict
  val linordered_semidom_linordered_idom :
    'a linordered_idom -> 'a linordered_semidom
  val linordered_semiring_1_strict_linordered_idom :
    'a linordered_idom -> 'a linordered_semiring_1_strict
  val ordered_comm_ring_linordered_idom :
    'a linordered_idom -> 'a ordered_comm_ring
  val ordered_ring_abs_linordered_idom :
    'a linordered_idom -> 'a ordered_ring_abs
  type 'a linordered_field
  val field_abs_sgn_linordered_field : 'a linordered_field -> 'a field_abs_sgn
  val field_char_0_linordered_field : 'a linordered_field -> 'a field_char_0
  val unbounded_dense_linorder_linordered_field :
    'a linordered_field -> 'a Orderings.unbounded_dense_linorder
  val linordered_idom_linordered_field :
    'a linordered_field -> 'a linordered_idom
  val nat : int -> nat
  val suc : nat -> nat
  val dvd : 'a HOL.equal * 'a semidom_modulo -> 'a -> 'a -> bool
  val power : 'a power -> 'a -> nat -> 'a
  val uminus_int : int -> int
  val zero_int : int
  val less_int : int -> int -> bool
  val abs_int : int -> int
  val one_int : int
  val sgn_int : int -> int
  val nat_of_integer : IntInf.int -> nat
  val plus_int : int -> int -> int
  val of_nat : 'a semiring_1 -> nat -> 'a
  val bit_cut_integer : IntInf.int -> IntInf.int * bool
  val minus_int : int -> int -> int
  val less_eq_int : int -> int -> bool
  val times_int : int -> int -> int
  val of_bool : 'a zero_neq_one -> bool -> 'a
  val divide_int : int -> int -> int
end = struct

datatype int = Int_of_integer of IntInf.int;

fun integer_of_int (Int_of_integer k) = k;

fun equal_inta k l = (((integer_of_int k) : IntInf.int) = (integer_of_int l));

val equal_int = {equal = equal_inta} : int HOL.equal;

datatype nat = Nat of IntInf.int;

fun integer_of_nat (Nat x) = x;

fun equal_nata m n = (((integer_of_nat m) : IntInf.int) = (integer_of_nat n));

val equal_nat = {equal = equal_nata} : nat HOL.equal;

fun times_nata m n = Nat (IntInf.* (integer_of_nat m, integer_of_nat n));

type 'a times = {times : 'a -> 'a -> 'a};
val times = #times : 'a times -> 'a -> 'a -> 'a;

type 'a dvd = {times_dvd : 'a times};
val times_dvd = #times_dvd : 'a dvd -> 'a times;

val times_nat = {times = times_nata} : nat times;

val dvd_nat = {times_dvd = times_nat} : nat dvd;

datatype num = One | Bit0 of num | Bit1 of num;

val one_nata : nat = Nat (1 : IntInf.int);

type 'a one = {one : 'a};
val one = #one : 'a one -> 'a;

val one_nat = {one = one_nata} : nat one;

fun plus_nata m n = Nat (IntInf.+ (integer_of_nat m, integer_of_nat n));

type 'a plus = {plus : 'a -> 'a -> 'a};
val plus = #plus : 'a plus -> 'a -> 'a -> 'a;

val plus_nat = {plus = plus_nata} : nat plus;

val zero_nata : nat = Nat (0 : IntInf.int);

type 'a zero = {zero : 'a};
val zero = #zero : 'a zero -> 'a;

val zero_nat = {zero = zero_nata} : nat zero;

type 'a semigroup_add = {plus_semigroup_add : 'a plus};
val plus_semigroup_add = #plus_semigroup_add : 'a semigroup_add -> 'a plus;

type 'a numeral =
  {one_numeral : 'a one, semigroup_add_numeral : 'a semigroup_add};
val one_numeral = #one_numeral : 'a numeral -> 'a one;
val semigroup_add_numeral = #semigroup_add_numeral :
  'a numeral -> 'a semigroup_add;

val semigroup_add_nat = {plus_semigroup_add = plus_nat} : nat semigroup_add;

val numeral_nat =
  {one_numeral = one_nat, semigroup_add_numeral = semigroup_add_nat} :
  nat numeral;

type 'a power = {one_power : 'a one, times_power : 'a times};
val one_power = #one_power : 'a power -> 'a one;
val times_power = #times_power : 'a power -> 'a times;

val power_nat = {one_power = one_nat, times_power = times_nat} : nat power;

val ord_integer =
  {less_eq = (fn a => fn b => IntInf.<= (a, b)),
    less = (fn a => fn b => IntInf.< (a, b))}
  : IntInf.int Orderings.ord;

fun minus_nata m n =
  Nat (Orderings.max ord_integer (0 : IntInf.int)
        (IntInf.- (integer_of_nat m, integer_of_nat n)));

type 'a minus = {minus : 'a -> 'a -> 'a};
val minus = #minus : 'a minus -> 'a -> 'a -> 'a;

val minus_nat = {minus = minus_nata} : nat minus;

fun divmod_integer k l =
  (if ((k : IntInf.int) = (0 : IntInf.int))
    then ((0 : IntInf.int), (0 : IntInf.int))
    else (if IntInf.< ((0 : IntInf.int), l)
           then (if IntInf.< ((0 : IntInf.int), k)
                  then IntInf.divMod (IntInf.abs k, IntInf.abs l)
                  else let
                         val (r, s) =
                           IntInf.divMod (IntInf.abs k, IntInf.abs l);
                       in
                         (if ((s : IntInf.int) = (0 : IntInf.int))
                           then (IntInf.~ r, (0 : IntInf.int))
                           else (IntInf.- (IntInf.~ r, (1 : IntInf.int)),
                                  IntInf.- (l, s)))
                       end)
           else (if ((l : IntInf.int) = (0 : IntInf.int))
                  then ((0 : IntInf.int), k)
                  else Product_Type.apsnd IntInf.~
                         (if IntInf.< (k, (0 : IntInf.int))
                           then IntInf.divMod (IntInf.abs k, IntInf.abs l)
                           else let
                                  val (r, s) =
                                    IntInf.divMod (IntInf.abs k, IntInf.abs l);
                                in
                                  (if ((s : IntInf.int) = (0 : IntInf.int))
                                    then (IntInf.~ r, (0 : IntInf.int))
                                    else (IntInf.- (IntInf.~
              r, (1 : IntInf.int)),
   IntInf.- (IntInf.~ l, s)))
                                end))));

fun divide_integer k l = Product_Type.fst (divmod_integer k l);

fun divide_nata m n =
  Nat (divide_integer (integer_of_nat m) (integer_of_nat n));

type 'a divide = {divide : 'a -> 'a -> 'a};
val divide = #divide : 'a divide -> 'a -> 'a -> 'a;

val divide_nat = {divide = divide_nata} : nat divide;

fun modulo_integer k l = Product_Type.snd (divmod_integer k l);

fun modulo_nata m n =
  Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));

type 'a modulo =
  {divide_modulo : 'a divide, dvd_modulo : 'a dvd, modulo : 'a -> 'a -> 'a};
val divide_modulo = #divide_modulo : 'a modulo -> 'a divide;
val dvd_modulo = #dvd_modulo : 'a modulo -> 'a dvd;
val modulo = #modulo : 'a modulo -> 'a -> 'a -> 'a;

val modulo_nat =
  {divide_modulo = divide_nat, dvd_modulo = dvd_nat, modulo = modulo_nata} :
  nat modulo;

fun less_eq_nat m n = IntInf.<= (integer_of_nat m, integer_of_nat n);

fun less_nat m n = IntInf.< (integer_of_nat m, integer_of_nat n);

val ord_nat = {less_eq = less_eq_nat, less = less_nat} : nat Orderings.ord;

type 'a ab_semigroup_add = {semigroup_add_ab_semigroup_add : 'a semigroup_add};
val semigroup_add_ab_semigroup_add = #semigroup_add_ab_semigroup_add :
  'a ab_semigroup_add -> 'a semigroup_add;

type 'a monoid_add =
  {semigroup_add_monoid_add : 'a semigroup_add, zero_monoid_add : 'a zero};
val semigroup_add_monoid_add = #semigroup_add_monoid_add :
  'a monoid_add -> 'a semigroup_add;
val zero_monoid_add = #zero_monoid_add : 'a monoid_add -> 'a zero;

type 'a comm_monoid_add =
  {ab_semigroup_add_comm_monoid_add : 'a ab_semigroup_add,
    monoid_add_comm_monoid_add : 'a monoid_add};
val ab_semigroup_add_comm_monoid_add = #ab_semigroup_add_comm_monoid_add :
  'a comm_monoid_add -> 'a ab_semigroup_add;
val monoid_add_comm_monoid_add = #monoid_add_comm_monoid_add :
  'a comm_monoid_add -> 'a monoid_add;

type 'a mult_zero = {times_mult_zero : 'a times, zero_mult_zero : 'a zero};
val times_mult_zero = #times_mult_zero : 'a mult_zero -> 'a times;
val zero_mult_zero = #zero_mult_zero : 'a mult_zero -> 'a zero;

type 'a semigroup_mult = {times_semigroup_mult : 'a times};
val times_semigroup_mult = #times_semigroup_mult :
  'a semigroup_mult -> 'a times;

type 'a semiring =
  {ab_semigroup_add_semiring : 'a ab_semigroup_add,
    semigroup_mult_semiring : 'a semigroup_mult};
val ab_semigroup_add_semiring = #ab_semigroup_add_semiring :
  'a semiring -> 'a ab_semigroup_add;
val semigroup_mult_semiring = #semigroup_mult_semiring :
  'a semiring -> 'a semigroup_mult;

type 'a semiring_0 =
  {comm_monoid_add_semiring_0 : 'a comm_monoid_add,
    mult_zero_semiring_0 : 'a mult_zero, semiring_semiring_0 : 'a semiring};
val comm_monoid_add_semiring_0 = #comm_monoid_add_semiring_0 :
  'a semiring_0 -> 'a comm_monoid_add;
val mult_zero_semiring_0 = #mult_zero_semiring_0 :
  'a semiring_0 -> 'a mult_zero;
val semiring_semiring_0 = #semiring_semiring_0 : 'a semiring_0 -> 'a semiring;

type 'a semiring_no_zero_divisors =
  {semiring_0_semiring_no_zero_divisors : 'a semiring_0};
val semiring_0_semiring_no_zero_divisors = #semiring_0_semiring_no_zero_divisors
  : 'a semiring_no_zero_divisors -> 'a semiring_0;

type 'a monoid_mult =
  {semigroup_mult_monoid_mult : 'a semigroup_mult,
    power_monoid_mult : 'a power};
val semigroup_mult_monoid_mult = #semigroup_mult_monoid_mult :
  'a monoid_mult -> 'a semigroup_mult;
val power_monoid_mult = #power_monoid_mult : 'a monoid_mult -> 'a power;

type 'a semiring_numeral =
  {monoid_mult_semiring_numeral : 'a monoid_mult,
    numeral_semiring_numeral : 'a numeral,
    semiring_semiring_numeral : 'a semiring};
val monoid_mult_semiring_numeral = #monoid_mult_semiring_numeral :
  'a semiring_numeral -> 'a monoid_mult;
val numeral_semiring_numeral = #numeral_semiring_numeral :
  'a semiring_numeral -> 'a numeral;
val semiring_semiring_numeral = #semiring_semiring_numeral :
  'a semiring_numeral -> 'a semiring;

type 'a zero_neq_one = {one_zero_neq_one : 'a one, zero_zero_neq_one : 'a zero};
val one_zero_neq_one = #one_zero_neq_one : 'a zero_neq_one -> 'a one;
val zero_zero_neq_one = #zero_zero_neq_one : 'a zero_neq_one -> 'a zero;

type 'a semiring_1 =
  {semiring_numeral_semiring_1 : 'a semiring_numeral,
    semiring_0_semiring_1 : 'a semiring_0,
    zero_neq_one_semiring_1 : 'a zero_neq_one};
val semiring_numeral_semiring_1 = #semiring_numeral_semiring_1 :
  'a semiring_1 -> 'a semiring_numeral;
val semiring_0_semiring_1 = #semiring_0_semiring_1 :
  'a semiring_1 -> 'a semiring_0;
val zero_neq_one_semiring_1 = #zero_neq_one_semiring_1 :
  'a semiring_1 -> 'a zero_neq_one;

type 'a semiring_1_no_zero_divisors =
  {semiring_1_semiring_1_no_zero_divisors : 'a semiring_1,
    semiring_no_zero_divisors_semiring_1_no_zero_divisors :
      'a semiring_no_zero_divisors};
val semiring_1_semiring_1_no_zero_divisors =
  #semiring_1_semiring_1_no_zero_divisors :
  'a semiring_1_no_zero_divisors -> 'a semiring_1;
val semiring_no_zero_divisors_semiring_1_no_zero_divisors =
  #semiring_no_zero_divisors_semiring_1_no_zero_divisors :
  'a semiring_1_no_zero_divisors -> 'a semiring_no_zero_divisors;

type 'a cancel_semigroup_add =
  {semigroup_add_cancel_semigroup_add : 'a semigroup_add};
val semigroup_add_cancel_semigroup_add = #semigroup_add_cancel_semigroup_add :
  'a cancel_semigroup_add -> 'a semigroup_add;

type 'a cancel_ab_semigroup_add =
  {ab_semigroup_add_cancel_ab_semigroup_add : 'a ab_semigroup_add,
    cancel_semigroup_add_cancel_ab_semigroup_add : 'a cancel_semigroup_add,
    minus_cancel_ab_semigroup_add : 'a minus};
val ab_semigroup_add_cancel_ab_semigroup_add =
  #ab_semigroup_add_cancel_ab_semigroup_add :
  'a cancel_ab_semigroup_add -> 'a ab_semigroup_add;
val cancel_semigroup_add_cancel_ab_semigroup_add =
  #cancel_semigroup_add_cancel_ab_semigroup_add :
  'a cancel_ab_semigroup_add -> 'a cancel_semigroup_add;
val minus_cancel_ab_semigroup_add = #minus_cancel_ab_semigroup_add :
  'a cancel_ab_semigroup_add -> 'a minus;

type 'a cancel_comm_monoid_add =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add : 'a cancel_ab_semigroup_add,
    comm_monoid_add_cancel_comm_monoid_add : 'a comm_monoid_add};
val cancel_ab_semigroup_add_cancel_comm_monoid_add =
  #cancel_ab_semigroup_add_cancel_comm_monoid_add :
  'a cancel_comm_monoid_add -> 'a cancel_ab_semigroup_add;
val comm_monoid_add_cancel_comm_monoid_add =
  #comm_monoid_add_cancel_comm_monoid_add :
  'a cancel_comm_monoid_add -> 'a comm_monoid_add;

type 'a semiring_0_cancel =
  {cancel_comm_monoid_add_semiring_0_cancel : 'a cancel_comm_monoid_add,
    semiring_0_semiring_0_cancel : 'a semiring_0};
val cancel_comm_monoid_add_semiring_0_cancel =
  #cancel_comm_monoid_add_semiring_0_cancel :
  'a semiring_0_cancel -> 'a cancel_comm_monoid_add;
val semiring_0_semiring_0_cancel = #semiring_0_semiring_0_cancel :
  'a semiring_0_cancel -> 'a semiring_0;

type 'a ab_semigroup_mult =
  {semigroup_mult_ab_semigroup_mult : 'a semigroup_mult};
val semigroup_mult_ab_semigroup_mult = #semigroup_mult_ab_semigroup_mult :
  'a ab_semigroup_mult -> 'a semigroup_mult;

type 'a comm_semiring =
  {ab_semigroup_mult_comm_semiring : 'a ab_semigroup_mult,
    semiring_comm_semiring : 'a semiring};
val ab_semigroup_mult_comm_semiring = #ab_semigroup_mult_comm_semiring :
  'a comm_semiring -> 'a ab_semigroup_mult;
val semiring_comm_semiring = #semiring_comm_semiring :
  'a comm_semiring -> 'a semiring;

type 'a comm_semiring_0 =
  {comm_semiring_comm_semiring_0 : 'a comm_semiring,
    semiring_0_comm_semiring_0 : 'a semiring_0};
val comm_semiring_comm_semiring_0 = #comm_semiring_comm_semiring_0 :
  'a comm_semiring_0 -> 'a comm_semiring;
val semiring_0_comm_semiring_0 = #semiring_0_comm_semiring_0 :
  'a comm_semiring_0 -> 'a semiring_0;

type 'a comm_semiring_0_cancel =
  {comm_semiring_0_comm_semiring_0_cancel : 'a comm_semiring_0,
    semiring_0_cancel_comm_semiring_0_cancel : 'a semiring_0_cancel};
val comm_semiring_0_comm_semiring_0_cancel =
  #comm_semiring_0_comm_semiring_0_cancel :
  'a comm_semiring_0_cancel -> 'a comm_semiring_0;
val semiring_0_cancel_comm_semiring_0_cancel =
  #semiring_0_cancel_comm_semiring_0_cancel :
  'a comm_semiring_0_cancel -> 'a semiring_0_cancel;

type 'a semiring_1_cancel =
  {semiring_0_cancel_semiring_1_cancel : 'a semiring_0_cancel,
    semiring_1_semiring_1_cancel : 'a semiring_1};
val semiring_0_cancel_semiring_1_cancel = #semiring_0_cancel_semiring_1_cancel :
  'a semiring_1_cancel -> 'a semiring_0_cancel;
val semiring_1_semiring_1_cancel = #semiring_1_semiring_1_cancel :
  'a semiring_1_cancel -> 'a semiring_1;

type 'a comm_monoid_mult =
  {ab_semigroup_mult_comm_monoid_mult : 'a ab_semigroup_mult,
    monoid_mult_comm_monoid_mult : 'a monoid_mult,
    dvd_comm_monoid_mult : 'a dvd};
val ab_semigroup_mult_comm_monoid_mult = #ab_semigroup_mult_comm_monoid_mult :
  'a comm_monoid_mult -> 'a ab_semigroup_mult;
val monoid_mult_comm_monoid_mult = #monoid_mult_comm_monoid_mult :
  'a comm_monoid_mult -> 'a monoid_mult;
val dvd_comm_monoid_mult = #dvd_comm_monoid_mult :
  'a comm_monoid_mult -> 'a dvd;

type 'a comm_semiring_1 =
  {comm_monoid_mult_comm_semiring_1 : 'a comm_monoid_mult,
    comm_semiring_0_comm_semiring_1 : 'a comm_semiring_0,
    semiring_1_comm_semiring_1 : 'a semiring_1};
val comm_monoid_mult_comm_semiring_1 = #comm_monoid_mult_comm_semiring_1 :
  'a comm_semiring_1 -> 'a comm_monoid_mult;
val comm_semiring_0_comm_semiring_1 = #comm_semiring_0_comm_semiring_1 :
  'a comm_semiring_1 -> 'a comm_semiring_0;
val semiring_1_comm_semiring_1 = #semiring_1_comm_semiring_1 :
  'a comm_semiring_1 -> 'a semiring_1;

type 'a comm_semiring_1_cancel =
  {comm_semiring_0_cancel_comm_semiring_1_cancel : 'a comm_semiring_0_cancel,
    comm_semiring_1_comm_semiring_1_cancel : 'a comm_semiring_1,
    semiring_1_cancel_comm_semiring_1_cancel : 'a semiring_1_cancel};
val comm_semiring_0_cancel_comm_semiring_1_cancel =
  #comm_semiring_0_cancel_comm_semiring_1_cancel :
  'a comm_semiring_1_cancel -> 'a comm_semiring_0_cancel;
val comm_semiring_1_comm_semiring_1_cancel =
  #comm_semiring_1_comm_semiring_1_cancel :
  'a comm_semiring_1_cancel -> 'a comm_semiring_1;
val semiring_1_cancel_comm_semiring_1_cancel =
  #semiring_1_cancel_comm_semiring_1_cancel :
  'a comm_semiring_1_cancel -> 'a semiring_1_cancel;

type 'a semidom =
  {comm_semiring_1_cancel_semidom : 'a comm_semiring_1_cancel,
    semiring_1_no_zero_divisors_semidom : 'a semiring_1_no_zero_divisors};
val comm_semiring_1_cancel_semidom = #comm_semiring_1_cancel_semidom :
  'a semidom -> 'a comm_semiring_1_cancel;
val semiring_1_no_zero_divisors_semidom = #semiring_1_no_zero_divisors_semidom :
  'a semidom -> 'a semiring_1_no_zero_divisors;

val ab_semigroup_add_nat = {semigroup_add_ab_semigroup_add = semigroup_add_nat}
  : nat ab_semigroup_add;

val monoid_add_nat =
  {semigroup_add_monoid_add = semigroup_add_nat, zero_monoid_add = zero_nat} :
  nat monoid_add;

val comm_monoid_add_nat =
  {ab_semigroup_add_comm_monoid_add = ab_semigroup_add_nat,
    monoid_add_comm_monoid_add = monoid_add_nat}
  : nat comm_monoid_add;

val mult_zero_nat = {times_mult_zero = times_nat, zero_mult_zero = zero_nat} :
  nat mult_zero;

val semigroup_mult_nat = {times_semigroup_mult = times_nat} :
  nat semigroup_mult;

val semiring_nat =
  {ab_semigroup_add_semiring = ab_semigroup_add_nat,
    semigroup_mult_semiring = semigroup_mult_nat}
  : nat semiring;

val semiring_0_nat =
  {comm_monoid_add_semiring_0 = comm_monoid_add_nat,
    mult_zero_semiring_0 = mult_zero_nat, semiring_semiring_0 = semiring_nat}
  : nat semiring_0;

val semiring_no_zero_divisors_nat =
  {semiring_0_semiring_no_zero_divisors = semiring_0_nat} :
  nat semiring_no_zero_divisors;

val monoid_mult_nat =
  {semigroup_mult_monoid_mult = semigroup_mult_nat,
    power_monoid_mult = power_nat}
  : nat monoid_mult;

val semiring_numeral_nat =
  {monoid_mult_semiring_numeral = monoid_mult_nat,
    numeral_semiring_numeral = numeral_nat,
    semiring_semiring_numeral = semiring_nat}
  : nat semiring_numeral;

val zero_neq_one_nat =
  {one_zero_neq_one = one_nat, zero_zero_neq_one = zero_nat} : nat zero_neq_one;

val semiring_1_nat =
  {semiring_numeral_semiring_1 = semiring_numeral_nat,
    semiring_0_semiring_1 = semiring_0_nat,
    zero_neq_one_semiring_1 = zero_neq_one_nat}
  : nat semiring_1;

val semiring_1_no_zero_divisors_nat =
  {semiring_1_semiring_1_no_zero_divisors = semiring_1_nat,
    semiring_no_zero_divisors_semiring_1_no_zero_divisors =
      semiring_no_zero_divisors_nat}
  : nat semiring_1_no_zero_divisors;

val cancel_semigroup_add_nat =
  {semigroup_add_cancel_semigroup_add = semigroup_add_nat} :
  nat cancel_semigroup_add;

val cancel_ab_semigroup_add_nat =
  {ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_nat,
    cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_nat,
    minus_cancel_ab_semigroup_add = minus_nat}
  : nat cancel_ab_semigroup_add;

val cancel_comm_monoid_add_nat =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add = cancel_ab_semigroup_add_nat,
    comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_nat}
  : nat cancel_comm_monoid_add;

val semiring_0_cancel_nat =
  {cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_nat,
    semiring_0_semiring_0_cancel = semiring_0_nat}
  : nat semiring_0_cancel;

val ab_semigroup_mult_nat =
  {semigroup_mult_ab_semigroup_mult = semigroup_mult_nat} :
  nat ab_semigroup_mult;

val comm_semiring_nat =
  {ab_semigroup_mult_comm_semiring = ab_semigroup_mult_nat,
    semiring_comm_semiring = semiring_nat}
  : nat comm_semiring;

val comm_semiring_0_nat =
  {comm_semiring_comm_semiring_0 = comm_semiring_nat,
    semiring_0_comm_semiring_0 = semiring_0_nat}
  : nat comm_semiring_0;

val comm_semiring_0_cancel_nat =
  {comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_nat,
    semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_nat}
  : nat comm_semiring_0_cancel;

val semiring_1_cancel_nat =
  {semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_nat,
    semiring_1_semiring_1_cancel = semiring_1_nat}
  : nat semiring_1_cancel;

val comm_monoid_mult_nat =
  {ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_nat,
    monoid_mult_comm_monoid_mult = monoid_mult_nat,
    dvd_comm_monoid_mult = dvd_nat}
  : nat comm_monoid_mult;

val comm_semiring_1_nat =
  {comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_nat,
    comm_semiring_0_comm_semiring_1 = comm_semiring_0_nat,
    semiring_1_comm_semiring_1 = semiring_1_nat}
  : nat comm_semiring_1;

val comm_semiring_1_cancel_nat =
  {comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_nat,
    comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_nat,
    semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_nat}
  : nat comm_semiring_1_cancel;

val semidom_nat =
  {comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_nat,
    semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_nat}
  : nat semidom;

val preorder_nat = {ord_preorder = ord_nat} : nat Orderings.preorder;

val order_nat = {preorder_order = preorder_nat} : nat Orderings.order;

val linorder_nat = {order_linorder = order_nat} : nat Orderings.linorder;

type 'a semiring_char_0 = {semiring_1_semiring_char_0 : 'a semiring_1};
val semiring_1_semiring_char_0 = #semiring_1_semiring_char_0 :
  'a semiring_char_0 -> 'a semiring_1;

val semiring_char_0_nat = {semiring_1_semiring_char_0 = semiring_1_nat} :
  nat semiring_char_0;

type 'a divide_trivial =
  {one_divide_trivial : 'a one, zero_divide_trivial : 'a zero,
    divide_divide_trivial : 'a divide};
val one_divide_trivial = #one_divide_trivial : 'a divide_trivial -> 'a one;
val zero_divide_trivial = #zero_divide_trivial : 'a divide_trivial -> 'a zero;
val divide_divide_trivial = #divide_divide_trivial :
  'a divide_trivial -> 'a divide;

val divide_trivial_nat =
  {one_divide_trivial = one_nat, zero_divide_trivial = zero_nat,
    divide_divide_trivial = divide_nat}
  : nat divide_trivial;

type 'a semiring_no_zero_divisors_cancel =
  {semiring_no_zero_divisors_semiring_no_zero_divisors_cancel :
     'a semiring_no_zero_divisors};
val semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
  #semiring_no_zero_divisors_semiring_no_zero_divisors_cancel :
  'a semiring_no_zero_divisors_cancel -> 'a semiring_no_zero_divisors;

type 'a semidom_divide =
  {divide_trivial_semidom_divide : 'a divide_trivial,
    semidom_semidom_divide : 'a semidom,
    semiring_no_zero_divisors_cancel_semidom_divide :
      'a semiring_no_zero_divisors_cancel};
val divide_trivial_semidom_divide = #divide_trivial_semidom_divide :
  'a semidom_divide -> 'a divide_trivial;
val semidom_semidom_divide = #semidom_semidom_divide :
  'a semidom_divide -> 'a semidom;
val semiring_no_zero_divisors_cancel_semidom_divide =
  #semiring_no_zero_divisors_cancel_semidom_divide :
  'a semidom_divide -> 'a semiring_no_zero_divisors_cancel;

val semiring_no_zero_divisors_cancel_nat =
  {semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
     semiring_no_zero_divisors_nat}
  : nat semiring_no_zero_divisors_cancel;

val semidom_divide_nat =
  {divide_trivial_semidom_divide = divide_trivial_nat,
    semidom_semidom_divide = semidom_nat,
    semiring_no_zero_divisors_cancel_semidom_divide =
      semiring_no_zero_divisors_cancel_nat}
  : nat semidom_divide;

type 'a semiring_modulo =
  {comm_semiring_1_cancel_semiring_modulo : 'a comm_semiring_1_cancel,
    modulo_semiring_modulo : 'a modulo};
val comm_semiring_1_cancel_semiring_modulo =
  #comm_semiring_1_cancel_semiring_modulo :
  'a semiring_modulo -> 'a comm_semiring_1_cancel;
val modulo_semiring_modulo = #modulo_semiring_modulo :
  'a semiring_modulo -> 'a modulo;

type 'a semiring_modulo_trivial =
  {divide_trivial_semiring_modulo_trivial : 'a divide_trivial,
    semiring_modulo_semiring_modulo_trivial : 'a semiring_modulo};
val divide_trivial_semiring_modulo_trivial =
  #divide_trivial_semiring_modulo_trivial :
  'a semiring_modulo_trivial -> 'a divide_trivial;
val semiring_modulo_semiring_modulo_trivial =
  #semiring_modulo_semiring_modulo_trivial :
  'a semiring_modulo_trivial -> 'a semiring_modulo;

type 'a algebraic_semidom =
  {semidom_divide_algebraic_semidom : 'a semidom_divide};
val semidom_divide_algebraic_semidom = #semidom_divide_algebraic_semidom :
  'a algebraic_semidom -> 'a semidom_divide;

type 'a semidom_modulo =
  {algebraic_semidom_semidom_modulo : 'a algebraic_semidom,
    semiring_modulo_trivial_semidom_modulo : 'a semiring_modulo_trivial};
val algebraic_semidom_semidom_modulo = #algebraic_semidom_semidom_modulo :
  'a semidom_modulo -> 'a algebraic_semidom;
val semiring_modulo_trivial_semidom_modulo =
  #semiring_modulo_trivial_semidom_modulo :
  'a semidom_modulo -> 'a semiring_modulo_trivial;

val semiring_modulo_nat =
  {comm_semiring_1_cancel_semiring_modulo = comm_semiring_1_cancel_nat,
    modulo_semiring_modulo = modulo_nat}
  : nat semiring_modulo;

val semiring_modulo_trivial_nat =
  {divide_trivial_semiring_modulo_trivial = divide_trivial_nat,
    semiring_modulo_semiring_modulo_trivial = semiring_modulo_nat}
  : nat semiring_modulo_trivial;

val algebraic_semidom_nat =
  {semidom_divide_algebraic_semidom = semidom_divide_nat} :
  nat algebraic_semidom;

val semidom_modulo_nat =
  {algebraic_semidom_semidom_modulo = algebraic_semidom_nat,
    semiring_modulo_trivial_semidom_modulo = semiring_modulo_trivial_nat}
  : nat semidom_modulo;

val one_integera : IntInf.int = (1 : IntInf.int);

val one_integer = {one = one_integera} : IntInf.int one;

val zero_integer = {zero = (0 : IntInf.int)} : IntInf.int zero;

val zero_neq_one_integer =
  {one_zero_neq_one = one_integer, zero_zero_neq_one = zero_integer} :
  IntInf.int zero_neq_one;

type 'a comm_semiring_1_cancel_crossproduct =
  {comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct :
     'a comm_semiring_1_cancel};
val comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
  #comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct :
  'a comm_semiring_1_cancel_crossproduct -> 'a comm_semiring_1_cancel;

type 'a uminus = {uminus : 'a -> 'a};
val uminus = #uminus : 'a uminus -> 'a -> 'a;

type 'a group_add =
  {cancel_semigroup_add_group_add : 'a cancel_semigroup_add,
    minus_group_add : 'a minus, monoid_add_group_add : 'a monoid_add,
    uminus_group_add : 'a uminus};
val cancel_semigroup_add_group_add = #cancel_semigroup_add_group_add :
  'a group_add -> 'a cancel_semigroup_add;
val minus_group_add = #minus_group_add : 'a group_add -> 'a minus;
val monoid_add_group_add = #monoid_add_group_add :
  'a group_add -> 'a monoid_add;
val uminus_group_add = #uminus_group_add : 'a group_add -> 'a uminus;

type 'a ab_group_add =
  {cancel_comm_monoid_add_ab_group_add : 'a cancel_comm_monoid_add,
    group_add_ab_group_add : 'a group_add};
val cancel_comm_monoid_add_ab_group_add = #cancel_comm_monoid_add_ab_group_add :
  'a ab_group_add -> 'a cancel_comm_monoid_add;
val group_add_ab_group_add = #group_add_ab_group_add :
  'a ab_group_add -> 'a group_add;

type 'a ring =
  {ab_group_add_ring : 'a ab_group_add,
    semiring_0_cancel_ring : 'a semiring_0_cancel};
val ab_group_add_ring = #ab_group_add_ring : 'a ring -> 'a ab_group_add;
val semiring_0_cancel_ring = #semiring_0_cancel_ring :
  'a ring -> 'a semiring_0_cancel;

type 'a ring_no_zero_divisors =
  {ring_ring_no_zero_divisors : 'a ring,
    semiring_no_zero_divisors_cancel_ring_no_zero_divisors :
      'a semiring_no_zero_divisors_cancel};
val ring_ring_no_zero_divisors = #ring_ring_no_zero_divisors :
  'a ring_no_zero_divisors -> 'a ring;
val semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
  #semiring_no_zero_divisors_cancel_ring_no_zero_divisors :
  'a ring_no_zero_divisors -> 'a semiring_no_zero_divisors_cancel;

type 'a neg_numeral =
  {group_add_neg_numeral : 'a group_add, numeral_neg_numeral : 'a numeral};
val group_add_neg_numeral = #group_add_neg_numeral :
  'a neg_numeral -> 'a group_add;
val numeral_neg_numeral = #numeral_neg_numeral : 'a neg_numeral -> 'a numeral;

type 'a ring_1 =
  {neg_numeral_ring_1 : 'a neg_numeral, ring_ring_1 : 'a ring,
    semiring_1_cancel_ring_1 : 'a semiring_1_cancel};
val neg_numeral_ring_1 = #neg_numeral_ring_1 : 'a ring_1 -> 'a neg_numeral;
val ring_ring_1 = #ring_ring_1 : 'a ring_1 -> 'a ring;
val semiring_1_cancel_ring_1 = #semiring_1_cancel_ring_1 :
  'a ring_1 -> 'a semiring_1_cancel;

type 'a ring_1_no_zero_divisors =
  {ring_1_ring_1_no_zero_divisors : 'a ring_1,
    ring_no_zero_divisors_ring_1_no_zero_divisors : 'a ring_no_zero_divisors,
    semiring_1_no_zero_divisors_ring_1_no_zero_divisors :
      'a semiring_1_no_zero_divisors};
val ring_1_ring_1_no_zero_divisors = #ring_1_ring_1_no_zero_divisors :
  'a ring_1_no_zero_divisors -> 'a ring_1;
val ring_no_zero_divisors_ring_1_no_zero_divisors =
  #ring_no_zero_divisors_ring_1_no_zero_divisors :
  'a ring_1_no_zero_divisors -> 'a ring_no_zero_divisors;
val semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
  #semiring_1_no_zero_divisors_ring_1_no_zero_divisors :
  'a ring_1_no_zero_divisors -> 'a semiring_1_no_zero_divisors;

type 'a comm_ring =
  {comm_semiring_0_cancel_comm_ring : 'a comm_semiring_0_cancel,
    ring_comm_ring : 'a ring};
val comm_semiring_0_cancel_comm_ring = #comm_semiring_0_cancel_comm_ring :
  'a comm_ring -> 'a comm_semiring_0_cancel;
val ring_comm_ring = #ring_comm_ring : 'a comm_ring -> 'a ring;

type 'a comm_ring_1 =
  {comm_ring_comm_ring_1 : 'a comm_ring,
    comm_semiring_1_cancel_comm_ring_1 : 'a comm_semiring_1_cancel,
    ring_1_comm_ring_1 : 'a ring_1};
val comm_ring_comm_ring_1 = #comm_ring_comm_ring_1 :
  'a comm_ring_1 -> 'a comm_ring;
val comm_semiring_1_cancel_comm_ring_1 = #comm_semiring_1_cancel_comm_ring_1 :
  'a comm_ring_1 -> 'a comm_semiring_1_cancel;
val ring_1_comm_ring_1 = #ring_1_comm_ring_1 : 'a comm_ring_1 -> 'a ring_1;

type 'a idom =
  {comm_ring_1_idom : 'a comm_ring_1,
    ring_1_no_zero_divisors_idom : 'a ring_1_no_zero_divisors,
    semidom_idom : 'a semidom,
    comm_semiring_1_cancel_crossproduct_idom :
      'a comm_semiring_1_cancel_crossproduct};
val comm_ring_1_idom = #comm_ring_1_idom : 'a idom -> 'a comm_ring_1;
val ring_1_no_zero_divisors_idom = #ring_1_no_zero_divisors_idom :
  'a idom -> 'a ring_1_no_zero_divisors;
val semidom_idom = #semidom_idom : 'a idom -> 'a semidom;
val comm_semiring_1_cancel_crossproduct_idom =
  #comm_semiring_1_cancel_crossproduct_idom :
  'a idom -> 'a comm_semiring_1_cancel_crossproduct;

type 'a idom_divide =
  {idom_idom_divide : 'a idom, semidom_divide_idom_divide : 'a semidom_divide};
val idom_idom_divide = #idom_idom_divide : 'a idom_divide -> 'a idom;
val semidom_divide_idom_divide = #semidom_divide_idom_divide :
  'a idom_divide -> 'a semidom_divide;

type 'a inverse = {divide_inverse : 'a divide, inverse : 'a -> 'a};
val divide_inverse = #divide_inverse : 'a inverse -> 'a divide;
val inverse = #inverse : 'a inverse -> 'a -> 'a;

type 'a division_ring =
  {inverse_division_ring : 'a inverse,
    divide_trivial_division_ring : 'a divide_trivial,
    ring_1_no_zero_divisors_division_ring : 'a ring_1_no_zero_divisors};
val inverse_division_ring = #inverse_division_ring :
  'a division_ring -> 'a inverse;
val divide_trivial_division_ring = #divide_trivial_division_ring :
  'a division_ring -> 'a divide_trivial;
val ring_1_no_zero_divisors_division_ring =
  #ring_1_no_zero_divisors_division_ring :
  'a division_ring -> 'a ring_1_no_zero_divisors;

type 'a field =
  {division_ring_field : 'a division_ring, idom_divide_field : 'a idom_divide};
val division_ring_field = #division_ring_field : 'a field -> 'a division_ring;
val idom_divide_field = #idom_divide_field : 'a field -> 'a idom_divide;

type 'a abs = {abs : 'a -> 'a};
val abs = #abs : 'a abs -> 'a -> 'a;

type 'a abs_if =
  {abs_abs_if : 'a abs, minus_abs_if : 'a minus, uminus_abs_if : 'a uminus,
    zero_abs_if : 'a zero, ord_abs_if : 'a Orderings.ord};
val abs_abs_if = #abs_abs_if : 'a abs_if -> 'a abs;
val minus_abs_if = #minus_abs_if : 'a abs_if -> 'a minus;
val uminus_abs_if = #uminus_abs_if : 'a abs_if -> 'a uminus;
val zero_abs_if = #zero_abs_if : 'a abs_if -> 'a zero;
val ord_abs_if = #ord_abs_if : 'a abs_if -> 'a Orderings.ord;

type 'a ring_char_0 =
  {semiring_char_0_ring_char_0 : 'a semiring_char_0,
    ring_1_ring_char_0 : 'a ring_1};
val semiring_char_0_ring_char_0 = #semiring_char_0_ring_char_0 :
  'a ring_char_0 -> 'a semiring_char_0;
val ring_1_ring_char_0 = #ring_1_ring_char_0 : 'a ring_char_0 -> 'a ring_1;

type 'a sgn = {sgn : 'a -> 'a};
val sgn = #sgn : 'a sgn -> 'a -> 'a;

type 'a idom_abs_sgn =
  {abs_idom_abs_sgn : 'a abs, sgn_idom_abs_sgn : 'a sgn,
    idom_idom_abs_sgn : 'a idom};
val abs_idom_abs_sgn = #abs_idom_abs_sgn : 'a idom_abs_sgn -> 'a abs;
val sgn_idom_abs_sgn = #sgn_idom_abs_sgn : 'a idom_abs_sgn -> 'a sgn;
val idom_idom_abs_sgn = #idom_idom_abs_sgn : 'a idom_abs_sgn -> 'a idom;

type 'a ordered_ab_semigroup_add =
  {ab_semigroup_add_ordered_ab_semigroup_add : 'a ab_semigroup_add,
    order_ordered_ab_semigroup_add : 'a Orderings.order};
val ab_semigroup_add_ordered_ab_semigroup_add =
  #ab_semigroup_add_ordered_ab_semigroup_add :
  'a ordered_ab_semigroup_add -> 'a ab_semigroup_add;
val order_ordered_ab_semigroup_add = #order_ordered_ab_semigroup_add :
  'a ordered_ab_semigroup_add -> 'a Orderings.order;

type 'a ordered_comm_monoid_add =
  {comm_monoid_add_ordered_comm_monoid_add : 'a comm_monoid_add,
    ordered_ab_semigroup_add_ordered_comm_monoid_add :
      'a ordered_ab_semigroup_add};
val comm_monoid_add_ordered_comm_monoid_add =
  #comm_monoid_add_ordered_comm_monoid_add :
  'a ordered_comm_monoid_add -> 'a comm_monoid_add;
val ordered_ab_semigroup_add_ordered_comm_monoid_add =
  #ordered_ab_semigroup_add_ordered_comm_monoid_add :
  'a ordered_comm_monoid_add -> 'a ordered_ab_semigroup_add;

type 'a ordered_semiring =
  {ordered_comm_monoid_add_ordered_semiring : 'a ordered_comm_monoid_add,
    semiring_ordered_semiring : 'a semiring};
val ordered_comm_monoid_add_ordered_semiring =
  #ordered_comm_monoid_add_ordered_semiring :
  'a ordered_semiring -> 'a ordered_comm_monoid_add;
val semiring_ordered_semiring = #semiring_ordered_semiring :
  'a ordered_semiring -> 'a semiring;

type 'a ordered_semiring_0 =
  {ordered_semiring_ordered_semiring_0 : 'a ordered_semiring,
    semiring_0_ordered_semiring_0 : 'a semiring_0};
val ordered_semiring_ordered_semiring_0 = #ordered_semiring_ordered_semiring_0 :
  'a ordered_semiring_0 -> 'a ordered_semiring;
val semiring_0_ordered_semiring_0 = #semiring_0_ordered_semiring_0 :
  'a ordered_semiring_0 -> 'a semiring_0;

type 'a ordered_cancel_semiring =
  {ordered_semiring_0_ordered_cancel_semiring : 'a ordered_semiring_0,
    semiring_0_cancel_ordered_cancel_semiring : 'a semiring_0_cancel};
val ordered_semiring_0_ordered_cancel_semiring =
  #ordered_semiring_0_ordered_cancel_semiring :
  'a ordered_cancel_semiring -> 'a ordered_semiring_0;
val semiring_0_cancel_ordered_cancel_semiring =
  #semiring_0_cancel_ordered_cancel_semiring :
  'a ordered_cancel_semiring -> 'a semiring_0_cancel;

type 'a strict_ordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add};
val ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
  #ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add :
  'a strict_ordered_ab_semigroup_add -> 'a ordered_ab_semigroup_add;

type 'a ordered_cancel_ab_semigroup_add =
  {cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
     'a cancel_ab_semigroup_add,
    strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
      'a strict_ordered_ab_semigroup_add};
val cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
  #cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
  'a ordered_cancel_ab_semigroup_add -> 'a cancel_ab_semigroup_add;
val strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
  #strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
  'a ordered_cancel_ab_semigroup_add -> 'a strict_ordered_ab_semigroup_add;

type 'a ordered_ab_semigroup_add_imp_le =
  {ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le :
     'a ordered_cancel_ab_semigroup_add};
val ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
  #ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le :
  'a ordered_ab_semigroup_add_imp_le -> 'a ordered_cancel_ab_semigroup_add;

type 'a strict_ordered_comm_monoid_add =
  {comm_monoid_add_strict_ordered_comm_monoid_add : 'a comm_monoid_add,
    strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add :
      'a strict_ordered_ab_semigroup_add};
val comm_monoid_add_strict_ordered_comm_monoid_add =
  #comm_monoid_add_strict_ordered_comm_monoid_add :
  'a strict_ordered_comm_monoid_add -> 'a comm_monoid_add;
val strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
  #strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add :
  'a strict_ordered_comm_monoid_add -> 'a strict_ordered_ab_semigroup_add;

type 'a ordered_cancel_comm_monoid_add =
  {ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add :
     'a ordered_cancel_ab_semigroup_add,
    ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a ordered_comm_monoid_add,
    strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a strict_ordered_comm_monoid_add};
val ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
  #ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add :
  'a ordered_cancel_comm_monoid_add -> 'a ordered_cancel_ab_semigroup_add;
val ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
  #ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
  'a ordered_cancel_comm_monoid_add -> 'a ordered_comm_monoid_add;
val strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
  #strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
  'a ordered_cancel_comm_monoid_add -> 'a strict_ordered_comm_monoid_add;

type 'a ordered_ab_semigroup_monoid_add_imp_le =
  {cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
     'a cancel_comm_monoid_add,
    ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_ab_semigroup_add_imp_le,
    ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_cancel_comm_monoid_add};
val cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
  #cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
  'a ordered_ab_semigroup_monoid_add_imp_le -> 'a cancel_comm_monoid_add;
val ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
  #ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le :
  'a ordered_ab_semigroup_monoid_add_imp_le ->
    'a ordered_ab_semigroup_add_imp_le;
val ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
  #ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
  'a ordered_ab_semigroup_monoid_add_imp_le ->
    'a ordered_cancel_comm_monoid_add;

type 'a ordered_ab_group_add =
  {ab_group_add_ordered_ab_group_add : 'a ab_group_add,
    ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add :
      'a ordered_ab_semigroup_monoid_add_imp_le};
val ab_group_add_ordered_ab_group_add = #ab_group_add_ordered_ab_group_add :
  'a ordered_ab_group_add -> 'a ab_group_add;
val ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
  #ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add :
  'a ordered_ab_group_add -> 'a ordered_ab_semigroup_monoid_add_imp_le;

type 'a ordered_ring =
  {ordered_ab_group_add_ordered_ring : 'a ordered_ab_group_add,
    ordered_cancel_semiring_ordered_ring : 'a ordered_cancel_semiring,
    ring_ordered_ring : 'a ring};
val ordered_ab_group_add_ordered_ring = #ordered_ab_group_add_ordered_ring :
  'a ordered_ring -> 'a ordered_ab_group_add;
val ordered_cancel_semiring_ordered_ring = #ordered_cancel_semiring_ordered_ring
  : 'a ordered_ring -> 'a ordered_cancel_semiring;
val ring_ordered_ring = #ring_ordered_ring : 'a ordered_ring -> 'a ring;

type 'a field_char_0 =
  {field_field_char_0 : 'a field, ring_char_0_field_char_0 : 'a ring_char_0};
val field_field_char_0 = #field_field_char_0 : 'a field_char_0 -> 'a field;
val ring_char_0_field_char_0 = #ring_char_0_field_char_0 :
  'a field_char_0 -> 'a ring_char_0;

type 'a zero_less_one =
  {order_zero_less_one : 'a Orderings.order,
    zero_neq_one_zero_less_one : 'a zero_neq_one};
val order_zero_less_one = #order_zero_less_one :
  'a zero_less_one -> 'a Orderings.order;
val zero_neq_one_zero_less_one = #zero_neq_one_zero_less_one :
  'a zero_less_one -> 'a zero_neq_one;

type 'a field_abs_sgn =
  {field_field_abs_sgn : 'a field,
    idom_abs_sgn_field_abs_sgn : 'a idom_abs_sgn};
val field_field_abs_sgn = #field_field_abs_sgn : 'a field_abs_sgn -> 'a field;
val idom_abs_sgn_field_abs_sgn = #idom_abs_sgn_field_abs_sgn :
  'a field_abs_sgn -> 'a idom_abs_sgn;

type 'a linordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_linordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add,
    linorder_linordered_ab_semigroup_add : 'a Orderings.linorder};
val ordered_ab_semigroup_add_linordered_ab_semigroup_add =
  #ordered_ab_semigroup_add_linordered_ab_semigroup_add :
  'a linordered_ab_semigroup_add -> 'a ordered_ab_semigroup_add;
val linorder_linordered_ab_semigroup_add = #linorder_linordered_ab_semigroup_add
  : 'a linordered_ab_semigroup_add -> 'a Orderings.linorder;

type 'a linordered_cancel_ab_semigroup_add =
  {linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add :
     'a linordered_ab_semigroup_add,
    ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add :
      'a ordered_ab_semigroup_add_imp_le};
val linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
  #linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add :
  'a linordered_cancel_ab_semigroup_add -> 'a linordered_ab_semigroup_add;
val ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
  #ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add :
  'a linordered_cancel_ab_semigroup_add -> 'a ordered_ab_semigroup_add_imp_le;

type 'a linordered_semiring =
  {linordered_cancel_ab_semigroup_add_linordered_semiring :
     'a linordered_cancel_ab_semigroup_add,
    ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring :
      'a ordered_ab_semigroup_monoid_add_imp_le,
    ordered_cancel_semiring_linordered_semiring : 'a ordered_cancel_semiring};
val linordered_cancel_ab_semigroup_add_linordered_semiring =
  #linordered_cancel_ab_semigroup_add_linordered_semiring :
  'a linordered_semiring -> 'a linordered_cancel_ab_semigroup_add;
val ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring =
  #ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring :
  'a linordered_semiring -> 'a ordered_ab_semigroup_monoid_add_imp_le;
val ordered_cancel_semiring_linordered_semiring =
  #ordered_cancel_semiring_linordered_semiring :
  'a linordered_semiring -> 'a ordered_cancel_semiring;

type 'a linordered_semiring_strict =
  {linordered_semiring_linordered_semiring_strict : 'a linordered_semiring};
val linordered_semiring_linordered_semiring_strict =
  #linordered_semiring_linordered_semiring_strict :
  'a linordered_semiring_strict -> 'a linordered_semiring;

type 'a linordered_semiring_1 =
  {linordered_semiring_linordered_semiring_1 : 'a linordered_semiring,
    semiring_1_linordered_semiring_1 : 'a semiring_1,
    zero_less_one_linordered_semiring_1 : 'a zero_less_one};
val linordered_semiring_linordered_semiring_1 =
  #linordered_semiring_linordered_semiring_1 :
  'a linordered_semiring_1 -> 'a linordered_semiring;
val semiring_1_linordered_semiring_1 = #semiring_1_linordered_semiring_1 :
  'a linordered_semiring_1 -> 'a semiring_1;
val zero_less_one_linordered_semiring_1 = #zero_less_one_linordered_semiring_1 :
  'a linordered_semiring_1 -> 'a zero_less_one;

type 'a linordered_semiring_1_strict =
  {linordered_semiring_1_linordered_semiring_1_strict :
     'a linordered_semiring_1,
    linordered_semiring_strict_linordered_semiring_1_strict :
      'a linordered_semiring_strict};
val linordered_semiring_1_linordered_semiring_1_strict =
  #linordered_semiring_1_linordered_semiring_1_strict :
  'a linordered_semiring_1_strict -> 'a linordered_semiring_1;
val linordered_semiring_strict_linordered_semiring_1_strict =
  #linordered_semiring_strict_linordered_semiring_1_strict :
  'a linordered_semiring_1_strict -> 'a linordered_semiring_strict;

type 'a ordered_ab_group_add_abs =
  {abs_ordered_ab_group_add_abs : 'a abs,
    ordered_ab_group_add_ordered_ab_group_add_abs : 'a ordered_ab_group_add};
val abs_ordered_ab_group_add_abs = #abs_ordered_ab_group_add_abs :
  'a ordered_ab_group_add_abs -> 'a abs;
val ordered_ab_group_add_ordered_ab_group_add_abs =
  #ordered_ab_group_add_ordered_ab_group_add_abs :
  'a ordered_ab_group_add_abs -> 'a ordered_ab_group_add;

type 'a linordered_ab_group_add =
  {linordered_cancel_ab_semigroup_add_linordered_ab_group_add :
     'a linordered_cancel_ab_semigroup_add,
    ordered_ab_group_add_linordered_ab_group_add : 'a ordered_ab_group_add};
val linordered_cancel_ab_semigroup_add_linordered_ab_group_add =
  #linordered_cancel_ab_semigroup_add_linordered_ab_group_add :
  'a linordered_ab_group_add -> 'a linordered_cancel_ab_semigroup_add;
val ordered_ab_group_add_linordered_ab_group_add =
  #ordered_ab_group_add_linordered_ab_group_add :
  'a linordered_ab_group_add -> 'a ordered_ab_group_add;

type 'a linordered_ring =
  {linordered_ab_group_add_linordered_ring : 'a linordered_ab_group_add,
    ordered_ab_group_add_abs_linordered_ring : 'a ordered_ab_group_add_abs,
    abs_if_linordered_ring : 'a abs_if,
    linordered_semiring_linordered_ring : 'a linordered_semiring,
    ordered_ring_linordered_ring : 'a ordered_ring};
val linordered_ab_group_add_linordered_ring =
  #linordered_ab_group_add_linordered_ring :
  'a linordered_ring -> 'a linordered_ab_group_add;
val ordered_ab_group_add_abs_linordered_ring =
  #ordered_ab_group_add_abs_linordered_ring :
  'a linordered_ring -> 'a ordered_ab_group_add_abs;
val abs_if_linordered_ring = #abs_if_linordered_ring :
  'a linordered_ring -> 'a abs_if;
val linordered_semiring_linordered_ring = #linordered_semiring_linordered_ring :
  'a linordered_ring -> 'a linordered_semiring;
val ordered_ring_linordered_ring = #ordered_ring_linordered_ring :
  'a linordered_ring -> 'a ordered_ring;

type 'a linordered_ring_strict =
  {linordered_ring_linordered_ring_strict : 'a linordered_ring,
    linordered_semiring_strict_linordered_ring_strict :
      'a linordered_semiring_strict,
    ring_no_zero_divisors_linordered_ring_strict : 'a ring_no_zero_divisors};
val linordered_ring_linordered_ring_strict =
  #linordered_ring_linordered_ring_strict :
  'a linordered_ring_strict -> 'a linordered_ring;
val linordered_semiring_strict_linordered_ring_strict =
  #linordered_semiring_strict_linordered_ring_strict :
  'a linordered_ring_strict -> 'a linordered_semiring_strict;
val ring_no_zero_divisors_linordered_ring_strict =
  #ring_no_zero_divisors_linordered_ring_strict :
  'a linordered_ring_strict -> 'a ring_no_zero_divisors;

type 'a ordered_comm_semiring =
  {comm_semiring_0_ordered_comm_semiring : 'a comm_semiring_0,
    ordered_semiring_ordered_comm_semiring : 'a ordered_semiring};
val comm_semiring_0_ordered_comm_semiring =
  #comm_semiring_0_ordered_comm_semiring :
  'a ordered_comm_semiring -> 'a comm_semiring_0;
val ordered_semiring_ordered_comm_semiring =
  #ordered_semiring_ordered_comm_semiring :
  'a ordered_comm_semiring -> 'a ordered_semiring;

type 'a ordered_cancel_comm_semiring =
  {comm_semiring_0_cancel_ordered_cancel_comm_semiring :
     'a comm_semiring_0_cancel,
    ordered_cancel_semiring_ordered_cancel_comm_semiring :
      'a ordered_cancel_semiring,
    ordered_comm_semiring_ordered_cancel_comm_semiring :
      'a ordered_comm_semiring};
val comm_semiring_0_cancel_ordered_cancel_comm_semiring =
  #comm_semiring_0_cancel_ordered_cancel_comm_semiring :
  'a ordered_cancel_comm_semiring -> 'a comm_semiring_0_cancel;
val ordered_cancel_semiring_ordered_cancel_comm_semiring =
  #ordered_cancel_semiring_ordered_cancel_comm_semiring :
  'a ordered_cancel_comm_semiring -> 'a ordered_cancel_semiring;
val ordered_comm_semiring_ordered_cancel_comm_semiring =
  #ordered_comm_semiring_ordered_cancel_comm_semiring :
  'a ordered_cancel_comm_semiring -> 'a ordered_comm_semiring;

type 'a linordered_comm_semiring_strict =
  {linordered_semiring_strict_linordered_comm_semiring_strict :
     'a linordered_semiring_strict,
    ordered_cancel_comm_semiring_linordered_comm_semiring_strict :
      'a ordered_cancel_comm_semiring};
val linordered_semiring_strict_linordered_comm_semiring_strict =
  #linordered_semiring_strict_linordered_comm_semiring_strict :
  'a linordered_comm_semiring_strict -> 'a linordered_semiring_strict;
val ordered_cancel_comm_semiring_linordered_comm_semiring_strict =
  #ordered_cancel_comm_semiring_linordered_comm_semiring_strict :
  'a linordered_comm_semiring_strict -> 'a ordered_cancel_comm_semiring;

type 'a linordered_nonzero_semiring =
  {semiring_char_0_linordered_nonzero_semiring : 'a semiring_char_0,
    linorder_linordered_nonzero_semiring : 'a Orderings.linorder,
    comm_semiring_1_linordered_nonzero_semiring : 'a comm_semiring_1,
    ordered_comm_semiring_linordered_nonzero_semiring :
      'a ordered_comm_semiring,
    zero_less_one_linordered_nonzero_semiring : 'a zero_less_one};
val semiring_char_0_linordered_nonzero_semiring =
  #semiring_char_0_linordered_nonzero_semiring :
  'a linordered_nonzero_semiring -> 'a semiring_char_0;
val linorder_linordered_nonzero_semiring = #linorder_linordered_nonzero_semiring
  : 'a linordered_nonzero_semiring -> 'a Orderings.linorder;
val comm_semiring_1_linordered_nonzero_semiring =
  #comm_semiring_1_linordered_nonzero_semiring :
  'a linordered_nonzero_semiring -> 'a comm_semiring_1;
val ordered_comm_semiring_linordered_nonzero_semiring =
  #ordered_comm_semiring_linordered_nonzero_semiring :
  'a linordered_nonzero_semiring -> 'a ordered_comm_semiring;
val zero_less_one_linordered_nonzero_semiring =
  #zero_less_one_linordered_nonzero_semiring :
  'a linordered_nonzero_semiring -> 'a zero_less_one;

type 'a linordered_semidom =
  {linordered_comm_semiring_strict_linordered_semidom :
     'a linordered_comm_semiring_strict,
    linordered_nonzero_semiring_linordered_semidom :
      'a linordered_nonzero_semiring,
    semidom_linordered_semidom : 'a semidom};
val linordered_comm_semiring_strict_linordered_semidom =
  #linordered_comm_semiring_strict_linordered_semidom :
  'a linordered_semidom -> 'a linordered_comm_semiring_strict;
val linordered_nonzero_semiring_linordered_semidom =
  #linordered_nonzero_semiring_linordered_semidom :
  'a linordered_semidom -> 'a linordered_nonzero_semiring;
val semidom_linordered_semidom = #semidom_linordered_semidom :
  'a linordered_semidom -> 'a semidom;

type 'a ordered_comm_ring =
  {comm_ring_ordered_comm_ring : 'a comm_ring,
    ordered_cancel_comm_semiring_ordered_comm_ring :
      'a ordered_cancel_comm_semiring,
    ordered_ring_ordered_comm_ring : 'a ordered_ring};
val comm_ring_ordered_comm_ring = #comm_ring_ordered_comm_ring :
  'a ordered_comm_ring -> 'a comm_ring;
val ordered_cancel_comm_semiring_ordered_comm_ring =
  #ordered_cancel_comm_semiring_ordered_comm_ring :
  'a ordered_comm_ring -> 'a ordered_cancel_comm_semiring;
val ordered_ring_ordered_comm_ring = #ordered_ring_ordered_comm_ring :
  'a ordered_comm_ring -> 'a ordered_ring;

type 'a ordered_ring_abs =
  {ordered_ab_group_add_abs_ordered_ring_abs : 'a ordered_ab_group_add_abs,
    ordered_ring_ordered_ring_abs : 'a ordered_ring};
val ordered_ab_group_add_abs_ordered_ring_abs =
  #ordered_ab_group_add_abs_ordered_ring_abs :
  'a ordered_ring_abs -> 'a ordered_ab_group_add_abs;
val ordered_ring_ordered_ring_abs = #ordered_ring_ordered_ring_abs :
  'a ordered_ring_abs -> 'a ordered_ring;

type 'a linordered_idom =
  {ring_char_0_linordered_idom : 'a ring_char_0,
    idom_abs_sgn_linordered_idom : 'a idom_abs_sgn,
    linordered_ring_strict_linordered_idom : 'a linordered_ring_strict,
    linordered_semidom_linordered_idom : 'a linordered_semidom,
    linordered_semiring_1_strict_linordered_idom :
      'a linordered_semiring_1_strict,
    ordered_comm_ring_linordered_idom : 'a ordered_comm_ring,
    ordered_ring_abs_linordered_idom : 'a ordered_ring_abs};
val ring_char_0_linordered_idom = #ring_char_0_linordered_idom :
  'a linordered_idom -> 'a ring_char_0;
val idom_abs_sgn_linordered_idom = #idom_abs_sgn_linordered_idom :
  'a linordered_idom -> 'a idom_abs_sgn;
val linordered_ring_strict_linordered_idom =
  #linordered_ring_strict_linordered_idom :
  'a linordered_idom -> 'a linordered_ring_strict;
val linordered_semidom_linordered_idom = #linordered_semidom_linordered_idom :
  'a linordered_idom -> 'a linordered_semidom;
val linordered_semiring_1_strict_linordered_idom =
  #linordered_semiring_1_strict_linordered_idom :
  'a linordered_idom -> 'a linordered_semiring_1_strict;
val ordered_comm_ring_linordered_idom = #ordered_comm_ring_linordered_idom :
  'a linordered_idom -> 'a ordered_comm_ring;
val ordered_ring_abs_linordered_idom = #ordered_ring_abs_linordered_idom :
  'a linordered_idom -> 'a ordered_ring_abs;

type 'a linordered_field =
  {field_abs_sgn_linordered_field : 'a field_abs_sgn,
    field_char_0_linordered_field : 'a field_char_0,
    unbounded_dense_linorder_linordered_field :
      'a Orderings.unbounded_dense_linorder,
    linordered_idom_linordered_field : 'a linordered_idom};
val field_abs_sgn_linordered_field = #field_abs_sgn_linordered_field :
  'a linordered_field -> 'a field_abs_sgn;
val field_char_0_linordered_field = #field_char_0_linordered_field :
  'a linordered_field -> 'a field_char_0;
val unbounded_dense_linorder_linordered_field =
  #unbounded_dense_linorder_linordered_field :
  'a linordered_field -> 'a Orderings.unbounded_dense_linorder;
val linordered_idom_linordered_field = #linordered_idom_linordered_field :
  'a linordered_field -> 'a linordered_idom;

fun nat k = Nat (Orderings.max ord_integer (0 : IntInf.int) (integer_of_int k));

fun suc n = plus_nata n one_nata;

fun dvd (A1_, A2_) a b =
  HOL.eq A1_
    (modulo
      ((modulo_semiring_modulo o semiring_modulo_semiring_modulo_trivial o
         semiring_modulo_trivial_semidom_modulo)
        A2_)
      b a)
    (zero ((zero_mult_zero o mult_zero_semiring_0 o semiring_0_semiring_1 o
             semiring_1_comm_semiring_1 o
             comm_semiring_1_comm_semiring_1_cancel o
             comm_semiring_1_cancel_semidom o semidom_semidom_divide o
             semidom_divide_algebraic_semidom o
             algebraic_semidom_semidom_modulo)
            A2_));

fun power A_ a n =
  (if equal_nata n zero_nata then one (one_power A_)
    else times (times_power A_) a (power A_ a (minus_nata n one_nata)));

fun uminus_int k = Int_of_integer (IntInf.~ (integer_of_int k));

val zero_int : int = Int_of_integer (0 : IntInf.int);

fun less_int k l = IntInf.< (integer_of_int k, integer_of_int l);

fun abs_int i = (if less_int i zero_int then uminus_int i else i);

val one_int : int = Int_of_integer (1 : IntInf.int);

fun sgn_int i =
  (if equal_inta i zero_int then zero_int
    else (if less_int zero_int i then one_int else uminus_int one_int));

fun numeral A_ (Bit1 n) =
  let
    val m = numeral A_ n;
  in
    plus ((plus_semigroup_add o semigroup_add_numeral) A_)
      (plus ((plus_semigroup_add o semigroup_add_numeral) A_) m m)
      (one (one_numeral A_))
  end
  | numeral A_ (Bit0 n) =
    let
      val m = numeral A_ n;
    in
      plus ((plus_semigroup_add o semigroup_add_numeral) A_) m m
    end
  | numeral A_ One = one (one_numeral A_);

fun nat_of_integer k = Nat (Orderings.max ord_integer (0 : IntInf.int) k);

fun divmod_nat m n =
  let
    val k = integer_of_nat m;
    val l = integer_of_nat n;
  in
    Product_Type.map_prod nat_of_integer nat_of_integer
      (if ((k : IntInf.int) = (0 : IntInf.int))
        then ((0 : IntInf.int), (0 : IntInf.int))
        else (if ((l : IntInf.int) = (0 : IntInf.int))
               then ((0 : IntInf.int), k)
               else IntInf.divMod (IntInf.abs k, IntInf.abs l)))
  end;

fun plus_int k l =
  Int_of_integer (IntInf.+ (integer_of_int k, integer_of_int l));

fun of_nat A_ n =
  (if equal_nata n zero_nata
    then zero ((zero_mult_zero o mult_zero_semiring_0 o semiring_0_semiring_1)
                A_)
    else let
           val (m, q) = divmod_nat n (nat_of_integer (2 : IntInf.int));
           val ma =
             times ((times_power o power_monoid_mult o
                      monoid_mult_semiring_numeral o
                      semiring_numeral_semiring_1)
                     A_)
               (numeral
                 ((numeral_semiring_numeral o semiring_numeral_semiring_1) A_)
                 (Bit0 One))
               (of_nat A_ m);
         in
           (if equal_nata q zero_nata then ma
             else plus ((plus_semigroup_add o semigroup_add_numeral o
                          numeral_semiring_numeral o
                          semiring_numeral_semiring_1)
                         A_)
                    ma (one ((one_numeral o numeral_semiring_numeral o
                               semiring_numeral_semiring_1)
                              A_)))
         end);

fun bit_cut_integer k =
  (if ((k : IntInf.int) = (0 : IntInf.int)) then ((0 : IntInf.int), false)
    else let
           val (r, s) =
             IntInf.divMod (IntInf.abs k, IntInf.abs (2 : IntInf.int));
         in
           ((if IntInf.< ((0 : IntInf.int), k) then r
              else IntInf.- (IntInf.~ r, s)),
             ((s : IntInf.int) = (1 : IntInf.int)))
         end);

fun minus_int k l =
  Int_of_integer (IntInf.- (integer_of_int k, integer_of_int l));

fun less_eq_int k l = IntInf.<= (integer_of_int k, integer_of_int l);

fun times_int k l =
  Int_of_integer (IntInf.* (integer_of_int k, integer_of_int l));

fun of_bool A_ true = one (one_zero_neq_one A_)
  | of_bool A_ false = zero (zero_zero_neq_one A_);

fun divide_int k l =
  Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));

end; (*struct Arith*)

structure GCD : sig
  val gcd_int : Arith.int -> Arith.int -> Arith.int
end = struct

fun gcd_integer k l =
  IntInf.abs
    (if ((l : IntInf.int) = (0 : IntInf.int)) then k
      else gcd_integer l (Arith.modulo_integer (IntInf.abs k) (IntInf.abs l)));

fun gcd_int (Arith.Int_of_integer x) (Arith.Int_of_integer y) =
  Arith.Int_of_integer (gcd_integer x y);

end; (*struct GCD*)

structure Rat : sig
  type rat
  val of_int : Arith.int -> rat
  val quotient_of : rat -> Arith.int * Arith.int
  val one_rat : rat
  val less_rat : rat -> rat -> bool
  val plus_rat : rat -> rat -> rat
  val zero_rat : rat
  val equal_rat : rat -> rat -> bool
  val minus_rat : rat -> rat -> rat
  val less_eq_rat : rat -> rat -> bool
  val times_rat : rat -> rat -> rat
  val divide_rat : rat -> rat -> rat
  val uminus_rat : rat -> rat
  val inverse_rat : rat -> rat
  val floor_rat : rat -> Arith.int
end = struct

datatype rat = Frct of (Arith.int * Arith.int);

fun of_int a = Frct (a, Arith.one_int);

fun normalize p =
  (if Arith.less_int Arith.zero_int (Product_Type.snd p)
    then let
           val a = GCD.gcd_int (Product_Type.fst p) (Product_Type.snd p);
         in
           (Arith.divide_int (Product_Type.fst p) a,
             Arith.divide_int (Product_Type.snd p) a)
         end
    else (if Arith.equal_inta (Product_Type.snd p) Arith.zero_int
           then (Arith.zero_int, Arith.one_int)
           else let
                  val a =
                    Arith.uminus_int
                      (GCD.gcd_int (Product_Type.fst p) (Product_Type.snd p));
                in
                  (Arith.divide_int (Product_Type.fst p) a,
                    Arith.divide_int (Product_Type.snd p) a)
                end));

fun quotient_of (Frct x) = x;

val one_rat : rat = Frct (Arith.one_int, Arith.one_int);

fun less_rat p q = let
                     val (a, c) = quotient_of p;
                     val (b, d) = quotient_of q;
                   in
                     Arith.less_int (Arith.times_int a d) (Arith.times_int c b)
                   end;

fun plus_rat p q =
  Frct let
         val (a, c) = quotient_of p;
         val (b, d) = quotient_of q;
       in
         normalize
           (Arith.plus_int (Arith.times_int a d) (Arith.times_int b c),
             Arith.times_int c d)
       end;

val zero_rat : rat = Frct (Arith.zero_int, Arith.one_int);

fun equal_rat a b =
  Product_Type.equal_prod Arith.equal_int Arith.equal_int (quotient_of a)
    (quotient_of b);

fun minus_rat p q =
  Frct let
         val (a, c) = quotient_of p;
         val (b, d) = quotient_of q;
       in
         normalize
           (Arith.minus_int (Arith.times_int a d) (Arith.times_int b c),
             Arith.times_int c d)
       end;

fun less_eq_rat p q =
  let
    val (a, c) = quotient_of p;
    val (b, d) = quotient_of q;
  in
    Arith.less_eq_int (Arith.times_int a d) (Arith.times_int c b)
  end;

fun times_rat p q = Frct let
                           val (a, c) = quotient_of p;
                           val (b, d) = quotient_of q;
                         in
                           normalize (Arith.times_int a b, Arith.times_int c d)
                         end;

fun divide_rat p q =
  Frct let
         val (a, c) = quotient_of p;
         val (b, d) = quotient_of q;
       in
         normalize (Arith.times_int a d, Arith.times_int c b)
       end;

fun uminus_rat p = Frct let
                          val (a, b) = quotient_of p;
                        in
                          (Arith.uminus_int a, b)
                        end;

fun inverse_rat p =
  Frct let
         val (a, b) = quotient_of p;
       in
         (if Arith.equal_inta a Arith.zero_int
           then (Arith.zero_int, Arith.one_int)
           else (Arith.times_int (Arith.sgn_int a) b, Arith.abs_int a))
       end;

fun floor_rat p = let
                    val (a, b) = quotient_of p;
                  in
                    Arith.divide_int a b
                  end;

end; (*struct Rat*)

structure Multiset : sig
  val part :
    'b Orderings.linorder ->
      ('a -> 'b) -> 'b -> 'a list -> 'a list * ('a list * 'a list)
end = struct

fun part B_ f pivot (x :: xs) =
  let
    val (lts, (eqs, gts)) = part B_ f pivot xs;
    val xa = f x;
  in
    (if Orderings.less
          ((Orderings.ord_preorder o Orderings.preorder_order o
             Orderings.order_linorder)
            B_)
          xa pivot
      then (x :: lts, (eqs, gts))
      else (if Orderings.less
                 ((Orderings.ord_preorder o Orderings.preorder_order o
                    Orderings.order_linorder)
                   B_)
                 pivot xa
             then (lts, (eqs, x :: gts)) else (lts, (x :: eqs, gts))))
  end
  | part B_ f pivot [] = ([], ([], []));

end; (*struct Multiset*)

structure List : sig
  val equal_list : 'a HOL.equal -> ('a list) HOL.equal
  val nth : 'a list -> Arith.nat -> 'a
  val upt : Arith.nat -> Arith.nat -> Arith.nat list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val find : ('a -> bool) -> 'a list -> 'a option
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val list_ex : ('a -> bool) -> 'a list -> bool
  val distinct : 'a HOL.equal -> 'a list -> bool
  val map : ('a -> 'b) -> 'a list -> 'b list
  val map_filter : ('a -> 'b option) -> 'a list -> 'b list
  val list_all : ('a -> bool) -> 'a list -> bool
  val size_list : 'a list -> Arith.nat
  val sort_key : 'b Orderings.linorder -> ('a -> 'b) -> 'a list -> 'a list
end = struct

fun equal_lista A_ [] (x21 :: x22) = false
  | equal_lista A_ (x21 :: x22) [] = false
  | equal_lista A_ (x21 :: x22) (y21 :: y22) =
    HOL.eq A_ x21 y21 andalso equal_lista A_ x22 y22
  | equal_lista A_ [] [] = true;

fun equal_list A_ = {equal = equal_lista A_} : ('a list) HOL.equal;

fun nth (x :: xs) n =
  (if Arith.equal_nata n Arith.zero_nata then x
    else nth xs (Arith.minus_nata n Arith.one_nata));

fun upt i j = (if Arith.less_nat i j then i :: upt (Arith.suc i) j else []);

fun zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
  | zip xs [] = []
  | zip [] ys = [];

fun find uu [] = NONE
  | find p (x :: xs) = (if p x then SOME x else find p xs);

fun fold f (x :: xs) s = fold f xs (f x s)
  | fold f [] s = s;

fun foldr f [] = Fun.id
  | foldr f (x :: xs) = f x o foldr f xs;

fun member A_ [] y = false
  | member A_ (x :: xs) y = HOL.eq A_ x y orelse member A_ xs y;

fun list_ex p [] = false
  | list_ex p (x :: xs) = p x orelse list_ex p xs;

fun distinct A_ [] = true
  | distinct A_ (x :: xs) = not (member A_ xs x) andalso distinct A_ xs;

fun map f [] = []
  | map f (x21 :: x22) = f x21 :: map f x22;

fun gen_length n (x :: xs) = gen_length (Arith.suc n) xs
  | gen_length n [] = n;

fun map_filter f [] = []
  | map_filter f (x :: xs) =
    (case f x of NONE => map_filter f xs | SOME y => y :: map_filter f xs);

fun list_all p [] = true
  | list_all p (x :: xs) = p x andalso list_all p xs;

fun size_list x = gen_length Arith.zero_nata x;

fun sort_key B_ f xs =
  (case xs of [] => [] | [_] => xs
    | [x, y] =>
      (if Orderings.less_eq
            ((Orderings.ord_preorder o Orderings.preorder_order o
               Orderings.order_linorder)
              B_)
            (f x) (f y)
        then xs else [y, x])
    | _ :: _ :: _ :: _ =>
      let
        val (lts, (eqs, gts)) =
          Multiset.part B_ f
            (f (nth xs
                 (Arith.divide_nata (size_list xs)
                   (Arith.nat_of_integer (2 : IntInf.int)))))
            xs;
      in
        sort_key B_ f lts @ eqs @ sort_key B_ f gts
      end);

end; (*struct List*)

structure Str : sig
  type char
  val implode : char list -> string
  val char_of_integer : IntInf.int -> char
end = struct

datatype char = Chara of bool * bool * bool * bool * bool * bool * bool * bool;

fun integer_of_char (Chara (b0, b1, b2, b3, b4, b5, b6, b7)) =
  IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (Arith.of_bool
                        Arith.zero_neq_one_integer
                        b7, (2 : IntInf.int)), Arith.of_bool
         Arith.zero_neq_one_integer
         b6), (2 : IntInf.int)), Arith.of_bool Arith.zero_neq_one_integer
                                   b5), (2 : IntInf.int)), Arith.of_bool
                     Arith.zero_neq_one_integer
                     b4), (2 : IntInf.int)), Arith.of_bool
       Arith.zero_neq_one_integer
       b3), (2 : IntInf.int)), Arith.of_bool Arith.zero_neq_one_integer
                                 b2), (2 : IntInf.int)), Arith.of_bool
                   Arith.zero_neq_one_integer
                   b1), (2 : IntInf.int)), Arith.of_bool
     Arith.zero_neq_one_integer b0);

fun implode cs = Str_Literal.literal_of_asciis (List.map integer_of_char cs);

fun char_of_integer k = let
                          val (q0, b0) = Arith.bit_cut_integer k;
                          val (q1, b1) = Arith.bit_cut_integer q0;
                          val (q2, b2) = Arith.bit_cut_integer q1;
                          val (q3, b3) = Arith.bit_cut_integer q2;
                          val (q4, b4) = Arith.bit_cut_integer q3;
                          val (q5, b5) = Arith.bit_cut_integer q4;
                          val (q6, b6) = Arith.bit_cut_integer q5;
                          val (_, a) = Arith.bit_cut_integer q6;
                        in
                          Chara (b0, b1, b2, b3, b4, b5, b6, a)
                        end;

end; (*struct Str*)

structure Archimedean_Field : sig
  type 'a archimedean_field
  type 'a floor_ceiling
  val archimedean_field_floor_ceiling : 'a floor_ceiling -> 'a archimedean_field
  val floor : 'a floor_ceiling -> 'a -> Arith.int
  val ceiling : 'a floor_ceiling -> 'a -> Arith.int
end = struct

type 'a archimedean_field =
  {linordered_field_archimedean_field : 'a Arith.linordered_field};
val linordered_field_archimedean_field = #linordered_field_archimedean_field :
  'a archimedean_field -> 'a Arith.linordered_field;

type 'a floor_ceiling =
  {archimedean_field_floor_ceiling : 'a archimedean_field,
    floor : 'a -> Arith.int};
val archimedean_field_floor_ceiling = #archimedean_field_floor_ceiling :
  'a floor_ceiling -> 'a archimedean_field;
val floor = #floor : 'a floor_ceiling -> 'a -> Arith.int;

fun ceiling A_ x =
  Arith.uminus_int
    (floor A_
      (Arith.uminus
        ((Arith.uminus_abs_if o Arith.abs_if_linordered_ring o
           Arith.linordered_ring_linordered_ring_strict o
           Arith.linordered_ring_strict_linordered_idom o
           Arith.linordered_idom_linordered_field o
           linordered_field_archimedean_field o archimedean_field_floor_ceiling)
          A_)
        x));

end; (*struct Archimedean_Field*)

structure Real : sig
  datatype real = Ratreal of Rat.rat
  val times_reala : real -> real -> real
  val zero_reala : real
  val less_real : real -> real -> bool
  val one_reala : real
  val minus_reala : real -> real -> real
  val plus_reala : real -> real -> real
  val monoid_add_real : real Arith.monoid_add
  val power_real : real Arith.power
  val semiring_1_real : real Arith.semiring_1
  val divide_reala : real -> real -> real
  val floor_ceiling_real : real Archimedean_Field.floor_ceiling
end = struct

datatype real = Ratreal of Rat.rat;

fun times_reala (Ratreal x) (Ratreal y) = Ratreal (Rat.times_rat x y);

val times_real = {times = times_reala} : real Arith.times;

val dvd_real = {times_dvd = times_real} : real Arith.dvd;

fun uminus_reala (Ratreal x) = Ratreal (Rat.uminus_rat x);

val zero_reala : real = Ratreal Rat.zero_rat;

fun less_real (Ratreal x) (Ratreal y) = Rat.less_rat x y;

fun abs_reala a = (if less_real a zero_reala then uminus_reala a else a);

val abs_real = {abs = abs_reala} : real Arith.abs;

val one_reala : real = Ratreal Rat.one_rat;

val one_real = {one = one_reala} : real Arith.one;

fun equal_real (Ratreal x) (Ratreal y) = Rat.equal_rat x y;

fun sgn_reala a =
  (if equal_real a zero_reala then zero_reala
    else (if less_real zero_reala a then one_reala
           else uminus_reala one_reala));

val sgn_real = {sgn = sgn_reala} : real Arith.sgn;

fun minus_reala (Ratreal x) (Ratreal y) = Ratreal (Rat.minus_rat x y);

fun plus_reala (Ratreal x) (Ratreal y) = Ratreal (Rat.plus_rat x y);

val plus_real = {plus = plus_reala} : real Arith.plus;

val semigroup_add_real = {plus_semigroup_add = plus_real} :
  real Arith.semigroup_add;

val cancel_semigroup_add_real =
  {semigroup_add_cancel_semigroup_add = semigroup_add_real} :
  real Arith.cancel_semigroup_add;

val ab_semigroup_add_real =
  {semigroup_add_ab_semigroup_add = semigroup_add_real} :
  real Arith.ab_semigroup_add;

val minus_real = {minus = minus_reala} : real Arith.minus;

val cancel_ab_semigroup_add_real =
  {ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_real,
    cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_real,
    minus_cancel_ab_semigroup_add = minus_real}
  : real Arith.cancel_ab_semigroup_add;

val zero_real = {zero = zero_reala} : real Arith.zero;

val monoid_add_real =
  {semigroup_add_monoid_add = semigroup_add_real, zero_monoid_add = zero_real} :
  real Arith.monoid_add;

val comm_monoid_add_real =
  {ab_semigroup_add_comm_monoid_add = ab_semigroup_add_real,
    monoid_add_comm_monoid_add = monoid_add_real}
  : real Arith.comm_monoid_add;

val cancel_comm_monoid_add_real =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add =
     cancel_ab_semigroup_add_real,
    comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_real}
  : real Arith.cancel_comm_monoid_add;

val mult_zero_real = {times_mult_zero = times_real, zero_mult_zero = zero_real}
  : real Arith.mult_zero;

val semigroup_mult_real = {times_semigroup_mult = times_real} :
  real Arith.semigroup_mult;

val semiring_real =
  {ab_semigroup_add_semiring = ab_semigroup_add_real,
    semigroup_mult_semiring = semigroup_mult_real}
  : real Arith.semiring;

val semiring_0_real =
  {comm_monoid_add_semiring_0 = comm_monoid_add_real,
    mult_zero_semiring_0 = mult_zero_real, semiring_semiring_0 = semiring_real}
  : real Arith.semiring_0;

val semiring_0_cancel_real =
  {cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_real,
    semiring_0_semiring_0_cancel = semiring_0_real}
  : real Arith.semiring_0_cancel;

val ab_semigroup_mult_real =
  {semigroup_mult_ab_semigroup_mult = semigroup_mult_real} :
  real Arith.ab_semigroup_mult;

val comm_semiring_real =
  {ab_semigroup_mult_comm_semiring = ab_semigroup_mult_real,
    semiring_comm_semiring = semiring_real}
  : real Arith.comm_semiring;

val comm_semiring_0_real =
  {comm_semiring_comm_semiring_0 = comm_semiring_real,
    semiring_0_comm_semiring_0 = semiring_0_real}
  : real Arith.comm_semiring_0;

val comm_semiring_0_cancel_real =
  {comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_real,
    semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_real}
  : real Arith.comm_semiring_0_cancel;

val power_real = {one_power = one_real, times_power = times_real} :
  real Arith.power;

val monoid_mult_real =
  {semigroup_mult_monoid_mult = semigroup_mult_real,
    power_monoid_mult = power_real}
  : real Arith.monoid_mult;

val numeral_real =
  {one_numeral = one_real, semigroup_add_numeral = semigroup_add_real} :
  real Arith.numeral;

val semiring_numeral_real =
  {monoid_mult_semiring_numeral = monoid_mult_real,
    numeral_semiring_numeral = numeral_real,
    semiring_semiring_numeral = semiring_real}
  : real Arith.semiring_numeral;

val zero_neq_one_real =
  {one_zero_neq_one = one_real, zero_zero_neq_one = zero_real} :
  real Arith.zero_neq_one;

val semiring_1_real =
  {semiring_numeral_semiring_1 = semiring_numeral_real,
    semiring_0_semiring_1 = semiring_0_real,
    zero_neq_one_semiring_1 = zero_neq_one_real}
  : real Arith.semiring_1;

val semiring_1_cancel_real =
  {semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_real,
    semiring_1_semiring_1_cancel = semiring_1_real}
  : real Arith.semiring_1_cancel;

val comm_monoid_mult_real =
  {ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_real,
    monoid_mult_comm_monoid_mult = monoid_mult_real,
    dvd_comm_monoid_mult = dvd_real}
  : real Arith.comm_monoid_mult;

val comm_semiring_1_real =
  {comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_real,
    comm_semiring_0_comm_semiring_1 = comm_semiring_0_real,
    semiring_1_comm_semiring_1 = semiring_1_real}
  : real Arith.comm_semiring_1;

val comm_semiring_1_cancel_real =
  {comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_real,
    comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_real,
    semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_real}
  : real Arith.comm_semiring_1_cancel;

val comm_semiring_1_cancel_crossproduct_real =
  {comm_semiring_1_cancel_comm_semiring_1_cancel_crossproduct =
     comm_semiring_1_cancel_real}
  : real Arith.comm_semiring_1_cancel_crossproduct;

val semiring_no_zero_divisors_real =
  {semiring_0_semiring_no_zero_divisors = semiring_0_real} :
  real Arith.semiring_no_zero_divisors;

val semiring_1_no_zero_divisors_real =
  {semiring_1_semiring_1_no_zero_divisors = semiring_1_real,
    semiring_no_zero_divisors_semiring_1_no_zero_divisors =
      semiring_no_zero_divisors_real}
  : real Arith.semiring_1_no_zero_divisors;

val semiring_no_zero_divisors_cancel_real =
  {semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
     semiring_no_zero_divisors_real}
  : real Arith.semiring_no_zero_divisors_cancel;

val uminus_real = {uminus = uminus_reala} : real Arith.uminus;

val group_add_real =
  {cancel_semigroup_add_group_add = cancel_semigroup_add_real,
    minus_group_add = minus_real, monoid_add_group_add = monoid_add_real,
    uminus_group_add = uminus_real}
  : real Arith.group_add;

val ab_group_add_real =
  {cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_real,
    group_add_ab_group_add = group_add_real}
  : real Arith.ab_group_add;

val ring_real =
  {ab_group_add_ring = ab_group_add_real,
    semiring_0_cancel_ring = semiring_0_cancel_real}
  : real Arith.ring;

val ring_no_zero_divisors_real =
  {ring_ring_no_zero_divisors = ring_real,
    semiring_no_zero_divisors_cancel_ring_no_zero_divisors =
      semiring_no_zero_divisors_cancel_real}
  : real Arith.ring_no_zero_divisors;

val neg_numeral_real =
  {group_add_neg_numeral = group_add_real, numeral_neg_numeral = numeral_real} :
  real Arith.neg_numeral;

val ring_1_real =
  {neg_numeral_ring_1 = neg_numeral_real, ring_ring_1 = ring_real,
    semiring_1_cancel_ring_1 = semiring_1_cancel_real}
  : real Arith.ring_1;

val ring_1_no_zero_divisors_real =
  {ring_1_ring_1_no_zero_divisors = ring_1_real,
    ring_no_zero_divisors_ring_1_no_zero_divisors = ring_no_zero_divisors_real,
    semiring_1_no_zero_divisors_ring_1_no_zero_divisors =
      semiring_1_no_zero_divisors_real}
  : real Arith.ring_1_no_zero_divisors;

val comm_ring_real =
  {comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_real,
    ring_comm_ring = ring_real}
  : real Arith.comm_ring;

val comm_ring_1_real =
  {comm_ring_comm_ring_1 = comm_ring_real,
    comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_real,
    ring_1_comm_ring_1 = ring_1_real}
  : real Arith.comm_ring_1;

val semidom_real =
  {comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_real,
    semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_real}
  : real Arith.semidom;

val idom_real =
  {comm_ring_1_idom = comm_ring_1_real,
    ring_1_no_zero_divisors_idom = ring_1_no_zero_divisors_real,
    semidom_idom = semidom_real,
    comm_semiring_1_cancel_crossproduct_idom =
      comm_semiring_1_cancel_crossproduct_real}
  : real Arith.idom;

fun inverse_reala (Ratreal x) = Ratreal (Rat.inverse_rat x);

fun divide_reala (Ratreal x) (Ratreal y) = Ratreal (Rat.divide_rat x y);

val divide_real = {divide = divide_reala} : real Arith.divide;

val divide_trivial_real =
  {one_divide_trivial = one_real, zero_divide_trivial = zero_real,
    divide_divide_trivial = divide_real}
  : real Arith.divide_trivial;

val inverse_real = {divide_inverse = divide_real, inverse = inverse_reala} :
  real Arith.inverse;

val division_ring_real =
  {inverse_division_ring = inverse_real,
    divide_trivial_division_ring = divide_trivial_real,
    ring_1_no_zero_divisors_division_ring = ring_1_no_zero_divisors_real}
  : real Arith.division_ring;

val semidom_divide_real =
  {divide_trivial_semidom_divide = divide_trivial_real,
    semidom_semidom_divide = semidom_real,
    semiring_no_zero_divisors_cancel_semidom_divide =
      semiring_no_zero_divisors_cancel_real}
  : real Arith.semidom_divide;

val idom_divide_real =
  {idom_idom_divide = idom_real,
    semidom_divide_idom_divide = semidom_divide_real}
  : real Arith.idom_divide;

val field_real =
  {division_ring_field = division_ring_real,
    idom_divide_field = idom_divide_real}
  : real Arith.field;

fun less_eq_real (Ratreal x) (Ratreal y) = Rat.less_eq_rat x y;

val ord_real = {less_eq = less_eq_real, less = less_real} : real Orderings.ord;

val abs_if_real =
  {abs_abs_if = abs_real, minus_abs_if = minus_real,
    uminus_abs_if = uminus_real, zero_abs_if = zero_real, ord_abs_if = ord_real}
  : real Arith.abs_if;

val semiring_char_0_real = {semiring_1_semiring_char_0 = semiring_1_real} :
  real Arith.semiring_char_0;

val ring_char_0_real =
  {semiring_char_0_ring_char_0 = semiring_char_0_real,
    ring_1_ring_char_0 = ring_1_real}
  : real Arith.ring_char_0;

val preorder_real = {ord_preorder = ord_real} : real Orderings.preorder;

val order_real = {preorder_order = preorder_real} : real Orderings.order;

val no_bot_real = {order_no_bot = order_real} : real Orderings.no_bot;

val no_top_real = {order_no_top = order_real} : real Orderings.no_top;

val linorder_real = {order_linorder = order_real} : real Orderings.linorder;

val idom_abs_sgn_real =
  {abs_idom_abs_sgn = abs_real, sgn_idom_abs_sgn = sgn_real,
    idom_idom_abs_sgn = idom_real}
  : real Arith.idom_abs_sgn;

val ordered_ab_semigroup_add_real =
  {ab_semigroup_add_ordered_ab_semigroup_add = ab_semigroup_add_real,
    order_ordered_ab_semigroup_add = order_real}
  : real Arith.ordered_ab_semigroup_add;

val ordered_comm_monoid_add_real =
  {comm_monoid_add_ordered_comm_monoid_add = comm_monoid_add_real,
    ordered_ab_semigroup_add_ordered_comm_monoid_add =
      ordered_ab_semigroup_add_real}
  : real Arith.ordered_comm_monoid_add;

val ordered_semiring_real =
  {ordered_comm_monoid_add_ordered_semiring = ordered_comm_monoid_add_real,
    semiring_ordered_semiring = semiring_real}
  : real Arith.ordered_semiring;

val ordered_semiring_0_real =
  {ordered_semiring_ordered_semiring_0 = ordered_semiring_real,
    semiring_0_ordered_semiring_0 = semiring_0_real}
  : real Arith.ordered_semiring_0;

val ordered_cancel_semiring_real =
  {ordered_semiring_0_ordered_cancel_semiring = ordered_semiring_0_real,
    semiring_0_cancel_ordered_cancel_semiring = semiring_0_cancel_real}
  : real Arith.ordered_cancel_semiring;

val strict_ordered_ab_semigroup_add_real =
  {ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
     ordered_ab_semigroup_add_real}
  : real Arith.strict_ordered_ab_semigroup_add;

val ordered_cancel_ab_semigroup_add_real =
  {cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
     cancel_ab_semigroup_add_real,
    strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
      strict_ordered_ab_semigroup_add_real}
  : real Arith.ordered_cancel_ab_semigroup_add;

val ordered_ab_semigroup_add_imp_le_real =
  {ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
     ordered_cancel_ab_semigroup_add_real}
  : real Arith.ordered_ab_semigroup_add_imp_le;

val strict_ordered_comm_monoid_add_real =
  {comm_monoid_add_strict_ordered_comm_monoid_add = comm_monoid_add_real,
    strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
      strict_ordered_ab_semigroup_add_real}
  : real Arith.strict_ordered_comm_monoid_add;

val ordered_cancel_comm_monoid_add_real =
  {ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
     ordered_cancel_ab_semigroup_add_real,
    ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
      ordered_comm_monoid_add_real,
    strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
      strict_ordered_comm_monoid_add_real}
  : real Arith.ordered_cancel_comm_monoid_add;

val ordered_ab_semigroup_monoid_add_imp_le_real =
  {cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
     cancel_comm_monoid_add_real,
    ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
      ordered_ab_semigroup_add_imp_le_real,
    ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
      ordered_cancel_comm_monoid_add_real}
  : real Arith.ordered_ab_semigroup_monoid_add_imp_le;

val ordered_ab_group_add_real =
  {ab_group_add_ordered_ab_group_add = ab_group_add_real,
    ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
      ordered_ab_semigroup_monoid_add_imp_le_real}
  : real Arith.ordered_ab_group_add;

val ordered_ring_real =
  {ordered_ab_group_add_ordered_ring = ordered_ab_group_add_real,
    ordered_cancel_semiring_ordered_ring = ordered_cancel_semiring_real,
    ring_ordered_ring = ring_real}
  : real Arith.ordered_ring;

val field_char_0_real =
  {field_field_char_0 = field_real, ring_char_0_field_char_0 = ring_char_0_real}
  : real Arith.field_char_0;

val zero_less_one_real =
  {order_zero_less_one = order_real,
    zero_neq_one_zero_less_one = zero_neq_one_real}
  : real Arith.zero_less_one;

val field_abs_sgn_real =
  {field_field_abs_sgn = field_real,
    idom_abs_sgn_field_abs_sgn = idom_abs_sgn_real}
  : real Arith.field_abs_sgn;

val dense_order_real = {order_dense_order = order_real} :
  real Orderings.dense_order;

val linordered_ab_semigroup_add_real =
  {ordered_ab_semigroup_add_linordered_ab_semigroup_add =
     ordered_ab_semigroup_add_real,
    linorder_linordered_ab_semigroup_add = linorder_real}
  : real Arith.linordered_ab_semigroup_add;

val linordered_cancel_ab_semigroup_add_real =
  {linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
     linordered_ab_semigroup_add_real,
    ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
      ordered_ab_semigroup_add_imp_le_real}
  : real Arith.linordered_cancel_ab_semigroup_add;

val linordered_semiring_real =
  {linordered_cancel_ab_semigroup_add_linordered_semiring =
     linordered_cancel_ab_semigroup_add_real,
    ordered_ab_semigroup_monoid_add_imp_le_linordered_semiring =
      ordered_ab_semigroup_monoid_add_imp_le_real,
    ordered_cancel_semiring_linordered_semiring = ordered_cancel_semiring_real}
  : real Arith.linordered_semiring;

val linordered_semiring_strict_real =
  {linordered_semiring_linordered_semiring_strict = linordered_semiring_real} :
  real Arith.linordered_semiring_strict;

val linordered_semiring_1_real =
  {linordered_semiring_linordered_semiring_1 = linordered_semiring_real,
    semiring_1_linordered_semiring_1 = semiring_1_real,
    zero_less_one_linordered_semiring_1 = zero_less_one_real}
  : real Arith.linordered_semiring_1;

val linordered_semiring_1_strict_real =
  {linordered_semiring_1_linordered_semiring_1_strict =
     linordered_semiring_1_real,
    linordered_semiring_strict_linordered_semiring_1_strict =
      linordered_semiring_strict_real}
  : real Arith.linordered_semiring_1_strict;

val ordered_ab_group_add_abs_real =
  {abs_ordered_ab_group_add_abs = abs_real,
    ordered_ab_group_add_ordered_ab_group_add_abs = ordered_ab_group_add_real}
  : real Arith.ordered_ab_group_add_abs;

val linordered_ab_group_add_real =
  {linordered_cancel_ab_semigroup_add_linordered_ab_group_add =
     linordered_cancel_ab_semigroup_add_real,
    ordered_ab_group_add_linordered_ab_group_add = ordered_ab_group_add_real}
  : real Arith.linordered_ab_group_add;

val linordered_ring_real =
  {linordered_ab_group_add_linordered_ring = linordered_ab_group_add_real,
    ordered_ab_group_add_abs_linordered_ring = ordered_ab_group_add_abs_real,
    abs_if_linordered_ring = abs_if_real,
    linordered_semiring_linordered_ring = linordered_semiring_real,
    ordered_ring_linordered_ring = ordered_ring_real}
  : real Arith.linordered_ring;

val linordered_ring_strict_real =
  {linordered_ring_linordered_ring_strict = linordered_ring_real,
    linordered_semiring_strict_linordered_ring_strict =
      linordered_semiring_strict_real,
    ring_no_zero_divisors_linordered_ring_strict = ring_no_zero_divisors_real}
  : real Arith.linordered_ring_strict;

val ordered_comm_semiring_real =
  {comm_semiring_0_ordered_comm_semiring = comm_semiring_0_real,
    ordered_semiring_ordered_comm_semiring = ordered_semiring_real}
  : real Arith.ordered_comm_semiring;

val ordered_cancel_comm_semiring_real =
  {comm_semiring_0_cancel_ordered_cancel_comm_semiring =
     comm_semiring_0_cancel_real,
    ordered_cancel_semiring_ordered_cancel_comm_semiring =
      ordered_cancel_semiring_real,
    ordered_comm_semiring_ordered_cancel_comm_semiring =
      ordered_comm_semiring_real}
  : real Arith.ordered_cancel_comm_semiring;

val linordered_comm_semiring_strict_real =
  {linordered_semiring_strict_linordered_comm_semiring_strict =
     linordered_semiring_strict_real,
    ordered_cancel_comm_semiring_linordered_comm_semiring_strict =
      ordered_cancel_comm_semiring_real}
  : real Arith.linordered_comm_semiring_strict;

val linordered_nonzero_semiring_real =
  {semiring_char_0_linordered_nonzero_semiring = semiring_char_0_real,
    linorder_linordered_nonzero_semiring = linorder_real,
    comm_semiring_1_linordered_nonzero_semiring = comm_semiring_1_real,
    ordered_comm_semiring_linordered_nonzero_semiring =
      ordered_comm_semiring_real,
    zero_less_one_linordered_nonzero_semiring = zero_less_one_real}
  : real Arith.linordered_nonzero_semiring;

val linordered_semidom_real =
  {linordered_comm_semiring_strict_linordered_semidom =
     linordered_comm_semiring_strict_real,
    linordered_nonzero_semiring_linordered_semidom =
      linordered_nonzero_semiring_real,
    semidom_linordered_semidom = semidom_real}
  : real Arith.linordered_semidom;

val ordered_comm_ring_real =
  {comm_ring_ordered_comm_ring = comm_ring_real,
    ordered_cancel_comm_semiring_ordered_comm_ring =
      ordered_cancel_comm_semiring_real,
    ordered_ring_ordered_comm_ring = ordered_ring_real}
  : real Arith.ordered_comm_ring;

val ordered_ring_abs_real =
  {ordered_ab_group_add_abs_ordered_ring_abs = ordered_ab_group_add_abs_real,
    ordered_ring_ordered_ring_abs = ordered_ring_real}
  : real Arith.ordered_ring_abs;

val linordered_idom_real =
  {ring_char_0_linordered_idom = ring_char_0_real,
    idom_abs_sgn_linordered_idom = idom_abs_sgn_real,
    linordered_ring_strict_linordered_idom = linordered_ring_strict_real,
    linordered_semidom_linordered_idom = linordered_semidom_real,
    linordered_semiring_1_strict_linordered_idom =
      linordered_semiring_1_strict_real,
    ordered_comm_ring_linordered_idom = ordered_comm_ring_real,
    ordered_ring_abs_linordered_idom = ordered_ring_abs_real}
  : real Arith.linordered_idom;

val dense_linorder_real =
  {dense_order_dense_linorder = dense_order_real,
    linorder_dense_linorder = linorder_real}
  : real Orderings.dense_linorder;

val unbounded_dense_linorder_real =
  {dense_linorder_unbounded_dense_linorder = dense_linorder_real,
    no_bot_unbounded_dense_linorder = no_bot_real,
    no_top_unbounded_dense_linorder = no_top_real}
  : real Orderings.unbounded_dense_linorder;

val linordered_field_real =
  {field_abs_sgn_linordered_field = field_abs_sgn_real,
    field_char_0_linordered_field = field_char_0_real,
    unbounded_dense_linorder_linordered_field = unbounded_dense_linorder_real,
    linordered_idom_linordered_field = linordered_idom_real}
  : real Arith.linordered_field;

fun floor_real (Ratreal x) = Rat.floor_rat x;

val archimedean_field_real =
  {linordered_field_archimedean_field = linordered_field_real} :
  real Archimedean_Field.archimedean_field;

val floor_ceiling_real =
  {archimedean_field_floor_ceiling = archimedean_field_real, floor = floor_real}
  : real Archimedean_Field.floor_ceiling;

end; (*struct Real*)

structure Set_Interval : sig
  val fold_atLeastAtMost_nat :
    (Arith.nat -> 'a -> 'a) -> Arith.nat -> Arith.nat -> 'a -> 'a
end = struct

fun fold_atLeastAtMost_nat f a b acc =
  (if Arith.less_nat b a then acc
    else fold_atLeastAtMost_nat f (Arith.plus_nata a Arith.one_nata) b
           (f a acc));

end; (*struct Set_Interval*)

structure Factorial : sig
  val fact : 'a Arith.semiring_char_0 -> Arith.nat -> 'a
end = struct

fun fact A_ n =
  Arith.of_nat (Arith.semiring_1_semiring_char_0 A_)
    (Set_Interval.fold_atLeastAtMost_nat Arith.times_nata
      (Arith.nat_of_integer (2 : IntInf.int)) n Arith.one_nata);

end; (*struct Factorial*)

structure Binomial : sig
  val binomial : Arith.nat -> Arith.nat -> Arith.nat
end = struct

fun binomial n k =
  (if Arith.less_nat n k then Arith.zero_nata
    else (if Arith.less_nat n
               (Arith.times_nata (Arith.nat_of_integer (2 : IntInf.int)) k)
           then binomial n (Arith.minus_nata n k)
           else Arith.divide_nata
                  (Set_Interval.fold_atLeastAtMost_nat Arith.times_nata
                    (Arith.plus_nata (Arith.minus_nata n k) Arith.one_nata) n
                    Arith.one_nata)
                  (Factorial.fact Arith.semiring_char_0_nat k)));

end; (*struct Binomial*)

structure Sum_Type : sig
  datatype ('a, 'b) sum = Inl of 'a | Inr of 'b
end = struct

datatype ('a, 'b) sum = Inl of 'a | Inr of 'b;

end; (*struct Sum_Type*)

structure CertCheck : sig
  val proj_sol : 'a list -> ('a -> bool) -> bool list
  val str_of_nat : Arith.nat -> string
  val str_of_bool : bool -> string
  val xor_from_bits : 'a list -> bool list * bool -> 'a list * bool
end = struct

fun proj_sol s w = List.map w s;

fun str_of_nat_aux n acc =
  let
    val c =
      Str.char_of_integer
        (Arith.integer_of_nat
          (Arith.plus_nata (Arith.nat_of_integer (48 : IntInf.int))
            (Arith.modulo_nata n (Arith.nat_of_integer (10 : IntInf.int)))));
  in
    (if Arith.less_nat n (Arith.nat_of_integer (10 : IntInf.int)) then c :: acc
      else str_of_nat_aux
             (Arith.divide_nata n (Arith.nat_of_integer (10 : IntInf.int)))
             (c :: acc))
  end;

fun str_of_nat n = Str.implode (str_of_nat_aux n []);

fun str_of_bool b = (if b then "true" else "false");

fun sublist_bits ls bs =
  List.map_filter
    (fn x => (if Product_Type.snd x then SOME (Product_Type.fst x) else NONE))
    (List.zip ls bs);

fun xor_from_bits v xsb =
  (sublist_bits v (Product_Type.fst xsb), Product_Type.snd xsb);

end; (*struct CertCheck*)

structure Groups_List : sig
  val sum_list : 'a Arith.monoid_add -> 'a list -> 'a
end = struct

fun sum_list A_ xs =
  List.foldr
    (Arith.plus
      ((Arith.plus_semigroup_add o Arith.semigroup_add_monoid_add) A_))
    xs (Arith.zero (Arith.zero_monoid_add A_));

end; (*struct Groups_List*)

structure ApproxMCAnalysis : sig
  val mk_eps : Real.real -> Real.real
  val compute_thresh : Real.real -> Arith.nat
  val raw_median_bound : Real.real -> Arith.nat -> Real.real
end = struct

fun mk_eps epsilon =
  (if Real.less_real Real.one_reala epsilon then Real.one_reala else epsilon);

fun compute_thresh epsilon =
  Arith.nat
    (Archimedean_Field.ceiling Real.floor_ceiling_real
      (Real.plus_reala Real.one_reala
        (Real.times_reala
          (Real.times_reala
            (Real.divide_reala
              (Real.Ratreal
                (Rat.of_int (Arith.Int_of_integer (984 : IntInf.int))))
              (Arith.power Real.power_real
                (Real.Ratreal
                  (Rat.of_int (Arith.Int_of_integer (10 : IntInf.int))))
                (Arith.nat_of_integer (2 : IntInf.int))))
            (Real.plus_reala Real.one_reala
              (Real.divide_reala epsilon
                (Real.plus_reala Real.one_reala epsilon))))
          (Arith.power Real.power_real
            (Real.plus_reala Real.one_reala
              (Real.divide_reala Real.one_reala epsilon))
            (Arith.nat_of_integer (2 : IntInf.int))))));

fun raw_median_bound alpha t =
  Groups_List.sum_list Real.monoid_add_real
    (List.map
      (fn i =>
        Real.times_reala
          (Real.times_reala
            (Arith.of_nat Real.semiring_1_real (Binomial.binomial t i))
            (Arith.power Real.power_real
              (Real.plus_reala
                (Real.divide_reala Real.one_reala
                  (Real.Ratreal
                    (Rat.of_int (Arith.Int_of_integer (2 : IntInf.int)))))
                alpha)
              i))
          (Arith.power Real.power_real
            (Real.minus_reala
              (Real.divide_reala Real.one_reala
                (Real.Ratreal
                  (Rat.of_int (Arith.Int_of_integer (2 : IntInf.int)))))
              alpha)
            (Arith.minus_nata t i)))
      (List.upt Arith.zero_nata
        (Arith.suc
          (Arith.divide_nata t (Arith.nat_of_integer (2 : IntInf.int))))));

end; (*struct ApproxMCAnalysis*)

structure CertCheck_CNF_EXT : sig
  datatype lit = Pos of Arith.nat | Neg of Arith.nat
  val find_t : Real.real -> Arith.nat
  val real_div : Real.real -> Real.real -> Real.real
  val certcheck :
    ((lit list) list *
       ((lit list) list * (lit list * (Arith.nat * lit)) list) ->
      bool) ->
      (lit list) list *
        ((lit list) list * (lit list * (Arith.nat * lit)) list) ->
        Arith.nat list ->
          Real.real ->
            Real.real ->
              (Arith.nat -> bool) list *
                (Arith.nat ->
                  Arith.nat *
                    ((Arith.nat -> bool) list * (Arith.nat -> bool) list)) ->
                (Arith.nat -> Arith.nat -> bool list * bool) ->
                  (string, Arith.nat) Sum_Type.sum
  val real_mult : Real.real -> Real.real -> Real.real
  val real_plus : Real.real -> Real.real -> Real.real
  val real_minus : Real.real -> Real.real -> Real.real
  val real_of_int : IntInf.int -> Real.real
end = struct

datatype lit = Pos of Arith.nat | Neg of Arith.nat;

fun sat_lit w l = (case l of Pos a => w a | Neg x => not (w x));

fun sat_cmsxor w c =
  not (Arith.dvd (Arith.equal_nat, Arith.semidom_modulo_nat)
        (Arith.nat_of_integer (2 : IntInf.int))
        (Groups_List.sum_list Arith.monoid_add_nat
          (List.map (Arith.of_bool Arith.zero_neq_one_nat o sat_lit w) c)));

fun sat_cmsbnn w (c, (k, y)) =
  Product_Type.equal_boola (sat_lit w y)
    (Arith.less_eq_nat k
      (Groups_List.sum_list Arith.monoid_add_nat
        (List.map (Arith.of_bool Arith.zero_neq_one_nat o sat_lit w) c)));

fun check_sol (fc, (fx, fb)) w =
  List.list_all (List.list_ex (sat_lit w)) fc andalso
    (List.list_all (sat_cmsxor w) fx andalso List.list_all (sat_cmsbnn w) fb);

fun ban_sol vs fml =
  (List.map (fn a => (case a of (v, true) => Neg v | (v, false) => Pos v)) vs ::
     Product_Type.fst fml,
    Product_Type.snd fml);

fun check_BSAT_sols check_unsat f s thresh cms =
  let
    val ps = List.map (CertCheck.proj_sol s) cms;
    val b1 = List.list_all (check_sol f) cms;
    val b2 = List.distinct (List.equal_list Product_Type.equal_bool) ps;
    val b3 =
      (if Arith.less_nat (List.size_list cms) thresh
        then check_unsat (List.fold (ban_sol o List.zip s) ps f) else true);
  in
    (if b1 andalso (b2 andalso b3) then Sum_Type.Inr ()
      else Sum_Type.Inl
             ("checks ---" ^ " all valid sols: " ^ CertCheck.str_of_bool b1 ^
                ", all distinct sols: " ^
                CertCheck.str_of_bool b2 ^
                ", unsat check (< thresh sols): " ^
               CertCheck.str_of_bool b3))
  end;

fun bsat check_unsat f s thresh xs =
  (case check_BSAT_sols check_unsat f s thresh xs
    of Sum_Type.Inl a => Sum_Type.Inl a
    | Sum_Type.Inr _ => Sum_Type.Inr (List.size_list xs));

fun fix_t _ = raise Fail "CertCheck_CNF_EXT.fix_t";

fun find_t delta =
  (case List.find
          (fn i =>
            Real.less_real
              (ApproxMCAnalysis.raw_median_bound
                (Real.divide_reala
                  (Real.Ratreal
                    (Rat.of_int (Arith.Int_of_integer (14 : IntInf.int))))
                  (Arith.power Real.power_real
                    (Real.Ratreal
                      (Rat.of_int (Arith.Int_of_integer (10 : IntInf.int))))
                    (Arith.nat_of_integer (2 : IntInf.int))))
                i)
              delta)
          (List.upt Arith.zero_nata (Arith.nat_of_integer (256 : IntInf.int)))
    of NONE => fix_t delta | SOME m => m);

fun enc_xor (x, b) (fc, (fx, fb)) =
  (if b then (fc, (List.map Pos x :: fx, fb))
    else (case x of [] => (fc, (fx, fb))
           | v :: vs => (fc, ((Neg v :: List.map Pos vs) :: fx, fb))));

fun real_div n m = Real.divide_reala n m;

fun size_xorL_cert check_unsat f s thresh xorsl i xs =
  let
    val xors =
      List.map (CertCheck.xor_from_bits s o xorsl) (List.upt Arith.zero_nata i);
    val fenc = List.fold enc_xor xors f;
  in
    bsat check_unsat fenc s thresh xs
  end;

fun approxcore_xorsL_cert check_unsat f s thresh (m, (cert1, cert2)) xorsl =
  (if Arith.less_eq_nat Arith.one_nata m andalso
        Arith.less_eq_nat m (List.size_list s)
    then (case size_xorL_cert check_unsat f s thresh xorsl
                 (Arith.minus_nata m Arith.one_nata) cert1
           of Sum_Type.Inl err => Sum_Type.Inl ("cert1 " ^ err)
           | Sum_Type.Inr n =>
             (if Arith.less_eq_nat thresh n
               then (if Arith.equal_nata m (List.size_list s)
                      then Sum_Type.Inr
                             (Arith.power Arith.power_nat
                               (Arith.nat_of_integer (2 : IntInf.int))
                               (List.size_list s))
                      else (case size_xorL_cert check_unsat f s thresh xorsl m
                                   cert2
                             of Sum_Type.Inl err =>
                               Sum_Type.Inl ("cert2 " ^ err)
                             | Sum_Type.Inr c =>
                               (if Arith.less_nat c thresh
                                 then Sum_Type.Inr
(Arith.times_nata
  (Arith.power Arith.power_nat (Arith.nat_of_integer (2 : IntInf.int)) m) c)
                                 else Sum_Type.Inl
"too many solutions at m added XORs")))
               else Sum_Type.Inl "too few solutions at m-1 added XORs"))
    else Sum_Type.Inl "invalid value of m, need 1 <= m <= |S|");

fun fold_approxcore_xorsL_cert check_unsat f s thresh t i cs xorsLs =
  (if Arith.equal_nata i Arith.zero_nata then Sum_Type.Inr []
    else let
           val it =
             Arith.minus_nata t (Arith.suc (Arith.minus_nata i Arith.one_nata));
         in
           (case approxcore_xorsL_cert check_unsat f s thresh (cs it)
                   (xorsLs it)
             of Sum_Type.Inl err =>
               Sum_Type.Inl ("round " ^ CertCheck.str_of_nat it ^ " " ^ err)
             | Sum_Type.Inr n =>
               (case fold_approxcore_xorsL_cert check_unsat f s thresh t
                       (Arith.minus_nata i Arith.one_nata) cs xorsLs
                 of Sum_Type.Inl a => Sum_Type.Inl a
                 | Sum_Type.Inr ns => Sum_Type.Inr (n :: ns)))
         end);

fun calc_median check_unsat f s thresh t ms xorsLs =
  (case fold_approxcore_xorsL_cert check_unsat f s thresh t t ms xorsLs
    of Sum_Type.Inl a => Sum_Type.Inl a
    | Sum_Type.Inr ls =>
      Sum_Type.Inr
        (List.nth (List.sort_key Arith.linorder_nat (fn x => x) ls)
          (Arith.divide_nata t (Arith.nat_of_integer (2 : IntInf.int)))));

fun certcheck check_unsat f s epsilon delta (m0, ms) xorsLs =
  (if Real.less_real Real.zero_reala delta andalso
        (Real.less_real delta Real.one_reala andalso
          (Real.less_real Real.zero_reala epsilon andalso
            List.distinct Arith.equal_nat s))
    then let
           val epsilona = ApproxMCAnalysis.mk_eps epsilon;
           val thresh = ApproxMCAnalysis.compute_thresh epsilona;
         in
           (case bsat check_unsat f s thresh m0
             of Sum_Type.Inl a => Sum_Type.Inl a
             | Sum_Type.Inr y =>
               (if Arith.less_nat y thresh then Sum_Type.Inr y
                 else let
                        val t = find_t delta;
                      in
                        calc_median check_unsat f s thresh t ms xorsLs
                      end))
         end
    else (raise Fail "invalid inputs")
           (fn _ => certcheck check_unsat f s epsilon delta (m0, ms) xorsLs));

fun real_mult n m = Real.times_reala n m;

fun real_plus n m = Real.plus_reala n m;

fun real_minus n m = Real.minus_reala n m;

fun real_of_int n = Arith.of_nat Real.semiring_1_real (Arith.nat_of_integer n);

end; (*struct CertCheck_CNF_EXT*)
