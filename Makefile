exe=arc.exe

SCM_SOURCES = arc-exe-init.scm ac.scm brackets.scm bitops.scm

$(exe): arc-exe.scm $(SCM_SOURCES)
	mzc --exe $@ $<

clean:
	-rm $(exe)

test check: $(exe)
	# This is not very meaningful because it is too wordy. Better run it
	# using runprove. See runtest here.
	find t/ -type f -name '*.arc.t' -exec ./$(exe) {} ';'

runtest runcheck: $(exe)
	bash Test.sh --exe

.PHONY: clean check test runtest runcheck
