genlin: compile.lisp genlin.lisp auxiliary.lisp tictactoe.lisp stackmachine.lisp params.lisp frontend.lisp
	sbcl --dynamic-space-size 2048 --script compile.lisp
# testing
# testing
data: datasets.tgz
	tar xzvf datasets.tgz

all: genlin data

clean: 
	tar czvf datasets.tgz datasets/
	rm -rf datasets GENLIN
