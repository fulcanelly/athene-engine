FROM alpine:3.13 as builder

RUN \
  apk add --no-cache \
    openssl-dev curl ghc cabal \
    gmp-dev build-base zlib-dev && \
  cabal v1-update

WORKDIR /build

COPY LICENSE athene-engine.cabal ./

RUN \
	cabal v1-sandbox init \
	&& cabal v1-install --only-dependencies -j4

COPY src/ src/

RUN cabal v1-build



FROM alpine:3.13 as app

RUN adduser -s /bin/sh -h /app -D haskell

RUN apk add --no-cache gmp libffi

WORKDIR /app
USER haskell

COPY --from=builder --chown=haskell \
	/build/dist/build/athene-engine/athene-engine .

CMD ["/app/athene-engine"]
