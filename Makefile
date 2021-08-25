all:
	./rebar3 compile

.PHONY: test

test:
	./rebar3 eunit --verbose

header:
	erl -pa ebin -noshell -s fix_template generate_headers -s init stop

parser:
	erl -pa ebin -noshell -s fix_template generate_parser -s init stop


clean:
	./rebar3 clean
