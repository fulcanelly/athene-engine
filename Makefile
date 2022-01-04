PY   := python
VENV := ./venv
SRC  := src.py/athene_bot

.PHONY: all
all: local-install

.PHONY: local-install
local-install: ${VENV}
	${VENV}/bin/pip install -e '.[dev]'

${VENV}:
	${PY} -m venv ${VENV}

.PHONY: mypy
mypy:
	${VENV}/bin/mypy ${SRC}

.PHONY: clean
clean:
	rm -rvf -- ${VENV}
