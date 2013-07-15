REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib crypto

dialyze: app.plt compile
	@$(DIALYZER) -q --plt app.plt ebin -Wunmatched_returns \
		-Werror_handling -Wrace_conditions -Wno_undefined_callbacks

test: compile
	@${REBAR} eunit skip_deps=true verbose=0

validate: dialyze test

clean:
	@$(RM) -rf deps/
	@$(REBAR) clean

.PHONY: all test clean validate dialyze deps
