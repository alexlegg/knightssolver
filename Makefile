all: server client

.PHONY: server
server:
	stack install

.PHONY: client
client:
	elm-make client/Main.elm --output=static/app.js
