SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v
DIST_DIR=dist
SBIN_DIR=$(DIST_DIR)/sbin

ERL_CMD=erl -pa $(EBIN_DIR) -sname griffin

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	erlc $(ERLC_OPTS) $<

clean:
	rm -f $(TARGETS)
	rm -f erl_crash.dump

run: all
	$(ERL_CMD) -noshell -s griffin start

shell: all
	$(ERL_CMD) -s griffin start

test: all
	$(ERL_CMD) -noshell -s griffin_tests test -s erlang halt

dist: all
	mkdir -p $(DIST_DIR)
	cp -r ebin include src $(DIST_DIR)
	rm -rf `find $(DIST_DIR) -name CVS`
	rm -f `find $(DIST_DIR) -name .cvsignore`
	mkdir -p $(SBIN_DIR)
	cp ../../LICENSE $(DIST_DIR)

distclean: clean
	rm -rf $(DIST_DIR)
	find . -name '*~' -exec rm {} \;
