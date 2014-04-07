try:
	for i in $$(ls examples/*.hs); do cat $$i; runghc $$i; done
cb:
	cabal build
