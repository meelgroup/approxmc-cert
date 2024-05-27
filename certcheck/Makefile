DEPS := Random.sig Random.sml helper.sml \
		certcheck_cnf_xor.ML parse_cnf_xor.sml

all: gen_rand certcheck_cnf_xor

gen_rand: $(DEPS) gen_rand.mlb gen_rand.sml
	mlton -link-opt '-static' -default-type intinf -output gen_rand gen_rand.mlb

certcheck_cnf_xor: $(DEPS) certcheck_cnf_xor.mlb certcheck_cnf_xor_main.sml
	mlton -link-opt '-static' -default-type intinf -output certcheck_cnf_xor certcheck_cnf_xor.mlb

clean:
	rm -fv gen_rand certcheck_cnf_xor
