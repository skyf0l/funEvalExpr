all:
	@stack build --copy-bins --local-bin-path ./

clean:
	@stack clean

fclean: clean
	@stack purge
	@rm -f ./funEvalExpr

re:
	@$(MAKE) -s fclean
	@$(MAKE) -s all

tests_run:
	@stack test

functional_tests_run:
	@./functional_tests_run.sh