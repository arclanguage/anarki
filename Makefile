exe=arc.exe

$(exe): arc-exe.scm
	mzc --exe $@ $^

clean:
	-rm $(exe)

test check: $(exe)
	find t/ -type f -name '*.arc.t' -exec ./$(exe) {} ';'

.PHONY: clean check test
