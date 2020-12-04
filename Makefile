TAG=lfe/revealjs
REVEAL_REPO=https://github.com/hakimel/reveal.js.git
REVEAL_DIR=reveal.js
PORT=1313
PRES_NAME=presentation
BUILD_DIR=$(shell pwd)/$(PRES_NAME)

default: build

build:
	@docker build -t $(TAG) .

run: build
	@docker run -it -p $(PORT):$(PORT) -v $(BUILD_DIR):/src $(TAG) server

new: build
	@docker run -it -p $(PORT):$(PORT) -v $(BUILD_DIR):/src $(TAG) new site $(PRES_NAME)

shell: build
	@docker run -it -v $(BUILD_DIR):/src --entrypoint=bash $(TAG)

clean-all:
	@-docker volume rm $$(docker volume ls -qf dangling=true)
	@-docker system prune
