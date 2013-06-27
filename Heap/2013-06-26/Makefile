hs := $(wildcard *.hs)
exe := $(hs:.hs=)
ps := Stage2.eps Stage3.eps Stage4.eps Stage5.eps
query := know between together

.PHONY: all clean
all: heap.pdf Stage1
clean:
	$(RM) heap.pdf heap.tex heap.tex~ $(wildcard Stage*pdf)
	$(RM) $(ps) $(exe) $(hs:.hs=.hi) $(hs:.hs=.aux) $(hs:.hs=.o)

heap.pdf: heap.org $(ps)
	emacs -Q $< -batch -f org-export-as-pdf

%.hp: %
	./$< docs $(query) +RTS -hc > /dev/null

$(exe): %: %.hs
	ghc --make -O2 -prof -fprof-auto $<

%.eps: %.hp
	hp2ps -c -e6in < $< > $@
