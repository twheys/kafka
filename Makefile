ERL=erl
EBIN=./ebin


all: clean compile
	cp ebin/* build/
	${ERL} -smp auto +A 64 +K true  -env ERL_MAX_ETS_TABLES 20000 -boot start_sasl -run goethe_app startapp -pa build

deps: 	
	mkdir -p deps
	rebar get-deps

compile: 	
	mkdir -p ebin
	rebar compile

node: compile
	mkdir -p rel 
	cd rel/; rebar create-node nodeid=goethe 
	echo '{sub_dirs, ["rel"]}.' > rebar.config

generate:
	rebar generate

clean:
	rm -f ebin/*
