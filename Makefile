APP       = gocaml
TEST      = gocaml_test
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt
OCAMLDEP  = ocamldep

INCLUDES     =             # all relevant -I options here
OCAMLFLAGS   = $(INCLUDES) # add other options for ocamlc here
OCAMLOPTFLAGS= $(INCLUDES) # add other options for ocamlopt here

OBJS        = utils.cmo type.cmo parser.cmo lexer.cmo main.cmo
TEST_OBJS   = utils.cmo type.cmo parser.cmo lexer.cmo test_main.cmo
DEPEND  	= utils.ml type.ml lexer.ml parser.ml parser.mli


all: .depend $(APP)

r: $(APP);
	make all
	./$(APP)
t:;
	./test.sh

ut:;
	make -f unit_test.mk ut

test: $(TEST);

include .depend

$(APP): $(OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(OBJS)

$(TEST): $(TEST_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(TEST_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

parser.ml parser.mli: parser.mly
	@rm -f $@
	ocamlyacc -v $<
	@chmod +w $@

lexer.ml: lexer.mll
	@rm -f $@
	ocamllex $<
	@chmod +w $@

.depend: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

c:;
	rm -f main
	rm -f *.cm[iox]
	rm -f *~
	rm -f lexer.ml
	rm -f gocaml gocaml_test
	rm -f unit_test *.o
	rm -f parser.ml parser.mli parser.output
