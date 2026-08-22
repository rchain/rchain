# Wrapper: generate the real Makefile from _CoqProject, then build.
# Requires `coq_makefile` on PATH. Install Coq with `opam install coq` or `apt install coq`.

COQMAKEFILE := CoqMakefile

all:
	coq_makefile -f _CoqProject -o $(COQMAKEFILE)
	$(MAKE) -f $(COQMAKEFILE)

clean:
	-$(MAKE) -f $(COQMAKEFILE) clean
	rm -f $(COQMAKEFILE) $(COQMAKEFILE).conf
