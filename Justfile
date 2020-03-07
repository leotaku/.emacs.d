# Commands

default: update lock

update:
	cd straight/repos; \
	parallel --timeout 30 --bar --halt-on-error soon,fail,1 'cd {}; git pull --quiet' ::: *

lock:
	emacs --batch -q -l ~/.emacs.d/load-packages.el --eval "(straight-freeze-versions t)"
