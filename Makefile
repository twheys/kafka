ERL=erl
EBIN=./ebin

compile: 
	rebar compile

goethe: compile
	${ERL} -smp auto +A 64 +K true  -env ERL_MAX_ETS_TABLES 20000 -boot start_sasl -run goethe_app startapp -pa ebin

clean:
	rebar clean
