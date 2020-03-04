tests: Spec
	./Spec

Spec: Spec.hs
	ghc -o Spec Spec.hs

setup-haskell:
	sudo apt-get install ghc libghc-hunit-dev

clean:
	rm Spec *.o *.hi
