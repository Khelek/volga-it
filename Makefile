REBAR = ./rebar

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile 

tests: 
	$(REBAR) -C rebar.config get-deps compile
	ERL_FLAGS="-config app.config -pa ebin deps/*/ebin" $(REBAR) -C rebar.config skip_deps=true eunit 

clean:
	$(REBAR) clean 

start:
	erl -sname server -setcookie mars -config app.config -pa ebin deps/*/ebin -s vis_request \
	-eval "io:format(\"Point your browser to http://localhost:PORT/ where PORT is as configured in app.config~n\")."

