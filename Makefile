all:
	./rebar compile

test:
	./rebar eunit

header:
	ERL_LIBS=../../deps erl -pa ebin -noshell -s fix_template generate_headers -s init stop

parser:
	ERL_LIBS=../../deps erl -pa ebin -noshell -s fix_template generate_parser -s init stop
