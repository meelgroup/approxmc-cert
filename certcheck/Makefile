DEPS := Random.sig Random.sml helper.sml \
		certcheck_cnf_ext.ML parse_cnf_ext.sml

all: gen_rand certcheck_cnf_ext

gen_rand: $(DEPS) gen_rand.mlb gen_rand.sml
	mlton -link-opt '-static' -default-type intinf -output gen_rand gen_rand.mlb

certcheck_cnf_ext: $(DEPS) certcheck_cnf_ext.mlb certcheck_cnf_ext_main.sml
	mlton -link-opt '-static' -default-type intinf -output certcheck_cnf_ext certcheck_cnf_ext.mlb

clean:
	rm -fv gen_rand certcheck_cnf_ext
