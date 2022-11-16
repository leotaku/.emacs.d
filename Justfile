# Commands

update: pull lock

pull:
	cd elpaca/repos; \
	parallel --timeout 30 --progress --tag --halt-on-error soon,fail,1 'cd "{}"; git -c core.askPass=true pull --rebase --autostash --quiet' ::: *

lock:
	emacs --batch -q -l ./load-packages.el --eval '(elpaca-write-lockfile (expand-file-name "lock.el" elpaca-directory))'
