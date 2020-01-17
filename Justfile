pull:
	cd straight/repos; \
	parallel --progress --bar --halt-on-error soon,fail,1 'cd {}; git pull' ::: *
