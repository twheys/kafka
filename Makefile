ERL=erl
EBIN=./ebin



compile: 	
	@mkdir -p ebin
	@rebar compile

node:
	@mkdir -p release
	@cd release/
	@rebar create-node nodeid=goethe

release:
	@rebar generate

goethe: clean compile
	@cp ebin/* build/
	${ERL} -smp auto +A 64 +K true  -env ERL_MAX_ETS_TABLES 20000 -boot start_sasl -run goethe_app startapp -pa build



clean:
	@rm -f ebin/*
