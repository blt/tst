REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
CTRUN=`which ct_run`

all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
dialyze: compile
	@$(DIALYZER) -q -n ebin -Wunmatched_returns -Werror_handling -Wrace_conditions
test: dialyze
# We don't use rebar's facilities because of a bug in rebar/proper
# interaction. Read more here: https://github.com/manopapad/proper/issues/14
	@-mkdir -p logs/
	@$(CTRUN) -pa ebin/ deps/*/ebin/ -dir test/ -logdir logs/
clean:
	@$(REBAR) clean

.PHONY: all test clean dialyze deps