
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

generate: clean-rel
	rebar generate
	cd rel; tar -cvf goethe.tar goethe

start: generate
	sudo chmod aog+x rel/goethe/bin/*
	sudo chmod aog+x rel/goethe/erts-5.9/bin/*
	sh rel/goethe/bin/goethe start

console: generate
	sudo chmod aog+x rel/goethe/bin/*
	sudo chmod aog+x rel/goethe/erts-5.9/bin/*
	sh rel/goethe/bin/goethe console
	
work-clean:
	sudo rm -rf /server/*

work: work-clean generate
	sudo cp rel/goethe.tar /server/
	cd /server; sudo tar xvf goethe.tar; sudo chmod aog+x goethe/bin/*; sudo chmod aog+x goethe/erts-5.9/bin/*; sh goethe/bin/goethe console

clean-rel:
	rm -rf rel/goethe
