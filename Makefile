TAG=lfe/revealjs
REVEAL_REPO=https://github.com/hakimel/reveal.js.git
REVEAL_DIR=reveal.js
PORT=1313
PRES_NAME=docker
CONTENT_DIR=$(shell pwd)/content

default: build

build:
	@docker build -t $(TAG) .

run: build
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	server

help: build
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	help

new: build
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	new site $(PRES_NAME)

shell: build
	@docker run -it -v $(CONTENT_DIR):/src/content --entrypoint=bash $(TAG)

clean-all:
	@-docker volume rm $$(docker volume ls -qf dangling=true)
	@-docker system prune
