.PHONY: compile test dialyzer clean

compile:
	@./rebar compile

test:
	@./rebar skip_deps=true eunit

dialyzer: build.plt compile
	dialyzer --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps kernel stdlib ssl --output_plt $@

clean:
	@./rebar clean
