exe=arc.exe

SCM_SOURCES = arc-exe-init.scm ac.scm brackets.scm bitops.scm

$(exe): arc-exe.scm $(SCM_SOURCES)
	mzc --exe $@ $<

# We need to delete the .arcc files because the ABI is not stable yet,
# and they may have some nasty leftovers. See:
#
# http://www.owlnet.rice.edu/~comp210/Handouts/SchemeTips.html

clean:
	-rm $(exe) *.arcc

test check: $(exe)
	perl Mini-Test.pl

runtest runcheck: $(exe)
	bash Test.sh --exe

.PHONY: clean check test runtest runcheck
