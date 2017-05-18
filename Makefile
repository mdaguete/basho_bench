REPO            ?= basho_bench

PKG_REVISION    ?= $(shell git describe --tags)
PKG_VERSION     ?= $(shell git describe --tags | tr - .)
PKG_ID           = basho-bench-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
OVERLAY_VARS    ?=

all: compile
	rebar3 escriptize
	@ln -sf _build/default/bin/basho_bench basho_bench

compile:
	rebar3 compile

run: compile
	rebar3 shell --apps basho_bench

.PHONY: compile

clean:
	rebar3 clean

distclean: clean
	@rm -rf basho_bench _build

results:
	Rscript --vanilla priv/summary.r -i tests/current

ops_sec-results: results

byte_sec-results:
	Rscript --vanilla priv/summary.r --ylabel1stgraph byte/sec -i tests/current

kb_sec-results:
	Rscript --vanilla priv/summary.r --ylabel1stgraph KB/sec -i tests/current

kib_sec-results:
	Rscript --vanilla priv/summary.r --ylabel1stgraph KiB/sec -i tests/current

mb_sec-results:
	Rscript --vanilla priv/summary.r --ylabel1stgraph MB/sec -i tests/current

mib_sec-results:
	Rscript --vanilla priv/summary.r --ylabel1stgraph MiB/sec -i tests/current

results-browser:
	cp -R priv/results-browser/* tests/current && cd tests/current && python -c 'import os, json; print json.dumps(os.listdir("."))' > web/data.json && python ../../priv/results-browser.py

TARGETS := $(shell ls tests/ | grep -v current)
JOBS := $(addprefix job,${TARGETS})
.PHONY: all_results ${JOBS}

all_results: ${JOBS} ; echo "$@ successfully generated."
${JOBS}: job%: ; Rscript --vanilla priv/summary.r -i tests/$*

