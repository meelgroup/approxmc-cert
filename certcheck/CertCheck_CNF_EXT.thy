section \<open> ApproxMC certification for CNF-XOR extended with theories \<close>

text \<open>
  This theory instantiates the locales with semantics for
  semantics for CNF-XOR extended with BNN, giving us a
  certificate checker for approximate counting in this theory.
\<close>

theory CertCheck_CNF_EXT imports
  "Approximate_Model_Counting.CertCheck"
  HOL.String "HOL-Library.Code_Target_Numeral"
  Show.Show_Real
  "Approximate_Model_Counting.ApproxMCAnalysis"
begin

text \<open>
  This follows CryptoMiniSAT's CNF-XOR-BNN formula syntax.
  A clause is a list of literals (one of which must be satisfied).
  An XOR constraint has the form $l_1 + l_2 + \dots + l_n = 1$ where addition is taken over $F_2$.
  A BNN constraint has the form $l_1 + l_2 + ... + l_n >= k <-> y$ with usual (nat) addition,
    k is a number and y is a litera.  
  Syntactically, the $l_i$ are specified by lists of LHS literals.
  Variables are natural numbers (in practice, variable 0 is never used)
\<close>

datatype lit = Pos nat | Neg nat
type_synonym clause = "lit list"
type_synonym cmsxor = "lit list"
type_synonym cmsbnn = "lit list \<times> nat \<times> lit"

type_synonym fml = "clause list \<times> cmsxor list \<times> cmsbnn list"

type_synonym assignment = "nat \<Rightarrow> bool"

definition sat_lit :: "assignment \<Rightarrow> lit \<Rightarrow> bool" where
  "sat_lit w l = (case l of Pos x \<Rightarrow> w x | Neg x \<Rightarrow> \<not>w x)"

definition sat_clause :: "assignment \<Rightarrow> clause \<Rightarrow> bool" where
  "sat_clause w C = (\<exists>l \<in> set C. sat_lit w l)"

definition sat_cmsxor :: "assignment \<Rightarrow> cmsxor \<Rightarrow> bool" where
  "sat_cmsxor w C = odd ((sum_list (map (of_bool \<circ> (sat_lit w)) C))::nat)"

fun sat_cmsbnn :: "assignment \<Rightarrow> cmsbnn \<Rightarrow> bool" where
  "sat_cmsbnn w (C,k,y) = (
     sat_lit w y \<longleftrightarrow> ((sum_list (map (of_bool \<circ> (sat_lit w)) C)::nat) \<ge> k))"

fun sat_fml :: "assignment \<Rightarrow> fml \<Rightarrow> bool"
  where
  "sat_fml w (fc,fx,fb) = (
    (\<forall>C \<in> set fc. sat_clause w C) \<and>
    (\<forall>C \<in> set fx. sat_cmsxor w C) \<and>
    (\<forall>C \<in> set fb. sat_cmsbnn w C)
  )"

(* The solution set *)
definition sols :: "fml \<Rightarrow> assignment set"
  where "sols f = {w. sat_fml w f}"

lemma sat_fml_cons[simp]:
  shows
  "sat_fml w (fc, x # fx, fb) \<longleftrightarrow>
  sat_fml w (fc,fx,fb) \<and> sat_cmsxor w x"
  "sat_fml w (c # fc,fx,fb) \<longleftrightarrow>
  sat_fml w (fc,fx,fb) \<and> sat_clause w c"
  by auto

(* Construct clauses for a given XOR *)
fun enc_xor :: "nat xor \<Rightarrow> fml \<Rightarrow> fml"
  where
    "enc_xor (x,b) (fc,fx,fb) = (
    if b then (fc, map Pos x # fx,fb)
    else
      case x of
        [] \<Rightarrow> (fc,fx,fb)
      | (v#vs) \<Rightarrow> (fc, (Neg v # map Pos vs) # fx,fb))"

lemma sols_enc_xor:
  shows "sols (enc_xor (x,b) (fc,fx,fb)) =
       sols (fc,fx,fb) \<inter> {\<omega>. satisfies_xorL (x,b) \<omega>}"
  unfolding sols_def
  by (cases x; auto simp add: satisfies_xorL_def sat_cmsxor_def o_def sat_lit_def list.case_eq_if)

(* Solution checking *)
fun check_sol :: "fml \<Rightarrow> (nat \<Rightarrow> bool) \<Rightarrow> bool"
  where "check_sol (fc,fx,fb) w = (
  list_all (list_ex (sat_lit w)) fc \<and>
  list_all (sat_cmsxor w) fx \<and>
  list_all (sat_cmsbnn w) fb)"

definition ban_sol :: "(nat \<times> bool) list \<Rightarrow> fml \<Rightarrow> fml"
  where "ban_sol vs fml =
    ((map (\<lambda>(v,b). if b then Neg v else Pos v) vs)#fst fml, snd fml)"

lemma check_sol_sol:
  shows "w \<in> sols F \<longleftrightarrow>
  check_sol F w"
  apply (cases F)
  unfolding sols_def
  by (clarsimp simp add:  Ball_set_list_all sat_clause_def  Bex_set_list_ex)

lemma ban_sat_clause:
  shows "sat_clause w (map (\<lambda>(v, b). if b then Neg v else Pos v) vs) \<longleftrightarrow>
    map w (map fst vs) \<noteq> map snd vs"
  unfolding sat_clause_def
  by (force simp add: sat_lit_def split: if_splits)

lemma sols_ban_sol:
  shows"sols (ban_sol vs F) =
       sols F \<inter>
       {\<omega>. map \<omega> (map fst vs) \<noteq> map snd vs}"
  unfolding ban_sol_def sols_def
  apply (cases F) by (auto simp add: ban_sat_clause)

(* Globally interpret the ApproxMC locale to make
    ApproxMC certification available for CNF-XOR *)
global_interpretation CertCheck_CNF_EXT :
  CertCheck "sols" "enc_xor" "check_sol" "ban_sol"
  defines
    random_seed_xors = CertCheck_CNF_EXT.random_seed_xors and
    fix_t = CertCheck_CNF_EXT.appmc.fix_t and
    find_t = CertCheck_CNF_EXT.find_t and
    BSAT = CertCheck_CNF_EXT.BSAT and
    check_BSAT_sols = CertCheck_CNF_EXT.check_BSAT_sols and
    size_xorL_cert = CertCheck_CNF_EXT.size_xorL_cert and
    approxcore_xorsL = CertCheck_CNF_EXT.approxcore_xorsL and
    fold_approxcore_xorsL_cert = CertCheck_CNF_EXT.fold_approxcore_xorsL_cert and
    approxcore_xorsL_cert = CertCheck_CNF_EXT.approxcore_xorsL_cert and
    calc_median = CertCheck_CNF_EXT.calc_median and
    certcheck = CertCheck_CNF_EXT.certcheck
  apply unfold_locales
  subgoal by (metis sols_enc_xor surj_pair)
  subgoal by (metis sols_ban_sol)
  by (metis check_sol_sol)

(* Note that we automatically get the associated theorems *)
thm CertCheck_CNF_EXT.certcheck_sound

thm CertCheck_CNF_EXT.certcheck_promise_complete

subsection \<open> Export code for a SML implementation. \<close>

definition real_of_int :: "integer \<Rightarrow> real"
  where "real_of_int n = real (nat_of_integer n)"

definition real_mult :: "real \<Rightarrow> real \<Rightarrow> real"
  where "real_mult n m = n * m"

definition real_div :: "real \<Rightarrow> real \<Rightarrow> real"
  where "real_div n m = n / m"

definition real_plus :: "real \<Rightarrow> real \<Rightarrow> real"
  where "real_plus n m = n + m"

definition real_minus :: "real \<Rightarrow> real \<Rightarrow> real"
  where "real_minus n m = n - m"

declare [[code abort: fix_t]]

(* Sample code export.
  Helper Isabelle converters, followed by exporting the actual code *)
export_code
  length
  nat_of_integer int_of_integer
  integer_of_nat integer_of_int
  real_of_int real_mult real_div real_plus real_minus
  quotient_of

  Pos Neg
  CertCheck_CNF_EXT.appmc.compute_thresh
  find_t certcheck
  in SML

end
