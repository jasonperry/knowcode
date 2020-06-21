OCAMLC=ocamlc
OBJDIR=../obj


# %.cmo: %.ml
#	ocamlc -c $< -o $(OBJDIR)/$@ # doesn't work, needs dependencies

# crummy. You can't stop it from putting .cm files in the current dir.
upperOnto: concept.ml upperOnto.ml
	$(OCAMLC) -o $(OBJDIR)/upperOnto concept.ml upperOnto.ml

all: upperOnto
