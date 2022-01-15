PY       := python
VENV     := ./venv
MOD_NAME := athene_bot
BIN      := ${VENV}/bin
SRC      := src.hy/${MOD_NAME}

#- functions -#
files-in = $(patsubst ${1}/%,%,$(wildcard ${1}/${2}))
hy-mod-name = ${MOD_NAME}.$(patsubst %.hy,%,\
	$(subst /,.,$(patsubst ${SRC}/%,%,${1})))

# TC - type check
TC_ROOT := .type-check
TC_DIR  := ${TC_ROOT}/${MOD_NAME}

TC_HY_SRC := $(call files-in,${SRC},*.hy)
TC_PY_SRC := $(call files-in,${SRC},*.py)
TC_HY_DST := $(addprefix ${TC_DIR}/,${TC_HY_SRC:.hy=.py})
TC_PY_DST := $(addprefix ${TC_DIR}/,${TC_PY_SRC})
TC_DST    := ${TC_HY_DST} ${TC_PY_DST}


.PHONY: all
all: local-install

.PHONY: local-install
local-install: ${VENV}
	${BIN}/pip install -e '.[dev]'

${VENV}:
	${PY} -m venv ${VENV}

.PHONY: type-check
type-check: ${TC_DST}
	${BIN}/mypy ${TC_DIR}

${TC_DIR}:
	mkdir -vp -- $@

${TC_HY_DST}: ${TC_DIR}/%.py: ${SRC}/%.hy ${TC_DIR}
	${BIN}/hy2py f -m $(call hy-mod-name,$<) $< > $@

${TC_PY_DST}: ${TC_DIR}/%: ${SRC}/% ${TC_DIR}
	cp -vf -- $< $@

.PHONY: clean-check
clean-check:
	rm -rf -- ./${TC_ROOT}

.PHONY: clean
clean: clean-check
	rm -rf -- ./${VENV}
