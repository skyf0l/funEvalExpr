all:
	@stack build --copy-bins

clean:
	@stack clean

fclean: clean
	@stack purge
	@rm -f ./funEvalExpr
	@rm -f ./funEvalPostfixExpr
	@rm -f ./funInfixToPostfixExpr

re:
	@$(MAKE) -s fclean
	@$(MAKE) -s all

tests_run:
	@stack test

functional_tests_run:
	@./functional_tests_run.sh