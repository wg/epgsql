NAME		:= epgsql
VERSION		:= $(shell git describe --always --tags)

ERL  		:= erl
ERLC 		:= erlc

# ------------------------------------------------------------------------

ERLC_FLAGS	:= -Wall -I include +debug_info

SRC			:= $(wildcard src/*.erl)
TESTS 		:= $(wildcard test_src/*.erl)
RELEASE		:= $(NAME)-$(VERSION).tar.gz

APPDIR		:= $(NAME)-$(VERSION)
BEAMS		:= $(SRC:src/%.erl=ebin/%.beam) 

compile: $(BEAMS) ebin/$(NAME).app

app: compile
	@mkdir -p $(APPDIR)/ebin
	@cp -r ebin/* $(APPDIR)/ebin/
	@cp -r include $(APPDIR)

release: app
	@tar czvf $(RELEASE) $(APPDIR)

clean:
	@rm -f ebin/*.beam
	@rm -f test_ebin/*.beam
	@rm -f ebin/$(NAME).app
	@rm -rf $(NAME)-$(VERSION) $(NAME)-*.tar.gz

test: $(TEST_SETUP_WITNESS) compile_tests
	$(ERL) -pa ebin/ -pa test_ebin/ -noshell -s pgsql_tests run_tests -s init stop

compile_tests: $(TESTS:test_src/%.erl=test_ebin/%.beam) compile

# ------------------------------------------------------------------------

.SUFFIXES: .erl .beam
.PHONY:    app compile compile_tests clean test

ebin/%.beam : src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<

ebin/%.app : src/%.app.src Makefile
	sed -e s/git/\"$(VERSION)\"/g $< > $@

test_ebin/%.beam : test_src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<
