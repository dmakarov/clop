PATH := /usr/local/bin:${PATH}
emacs ?= emacs

all:
	emacs -Q --batch -l build.el -f do-export

clean:
	git clean -fdx
