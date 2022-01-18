# athene-engine
the telegram bot for auto mutual channel advertising

### todo list
  - [x] state restoring
  - [ ] lazy i18
  - [ ] notification queue
  - [ ] post reviewing
  - [ ] adjustable content
  - [ ] link check
  - [ ] spam protection

### tips

- to easier run on client side use this version manager: [ghcup](https://www.haskell.org/ghcup/)


# athene-py-bot

## usage
```sh
python -m venv venv  # create virtual env

. ./venv/bin/activate

pip install -e .

athene-py-bot -h
```

## server

by default listen on `127.0.0.1:42069`

### request format
any request is command name and argument next to it without any delimiter

### response format
* on success `OK:` and response text
* on error `ERR:` and error description

### available commands

#### valid-channel?

examples:
* `valid-channel?@public_channel`
* `valid-channel?https://t.me/+private_hash`

responses:
* `y` if channel is valid
* `n` otherwise

#### reload-channels-list
reset interval and recollect statistics

accept no arguments

### exaples
```sh
echo 'valid-channel?@picbtw' | nc 127.0.0.1 42069
# OK:y

printf '%s' 'valid-channel?@pic' | nc 127.0.0.1 42069
# OK:n

echo 'wtf' | nc 127.0.0.1 42069
# ERR:invlid request
```

