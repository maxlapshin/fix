all:
	./rebar compile

.PHONY: test

test:
	./rebar eunit -v

header:
	erl -pa ebin -noshell -s fix_template generate_headers -s init stop

parser:
	erl -pa ebin -noshell -s fix_template generate_parser -s init stop


clean:
	./rebar clean
