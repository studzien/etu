.PHONY: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

plt: compile
	dialyzer --build_plt --apps kernel stdlib -r ./ebin --output_plt etu.plt

dialyze: compile
	dialyzer ./ebin --plts etu.plt -Wunmatched_returns -Werror_handling -Wunderspecs
