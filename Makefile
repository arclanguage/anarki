exe=arc.exe

SCM_SOURCES = arc-exe-init.scm ac.scm brackets.scm bitops.scm

$(exe): arc-exe.scm $(SCM_SOURCES)
	mzc --exe $@ $<

clean:
	-rm $(exe)

test check: $(exe)
	find t/ -type f -name '*.arc.t' -exec ./$(exe) {} ';'

.PHONY: clean check test
