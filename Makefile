all:
	@stack build --copy-bins

clean:
	@stack clean

fclean: clean
	@stack purge
	@rm -f ./funEvalExpr

re:
	@$(MAKE) -s fclean
	@$(MAKE) -s all

tests_run: all
	@./test_evalexpr.sh ./funEvalExpr