TAG=lfe/revealjs
REVEAL_REPO=https://github.com/hakimel/reveal.js.git
REVEAL_DIR=reveal.js
PORT=1313
PRES_NAME=docker
PWD=$(shell pwd)
CONTENT_DIR=$(PWD)/content
THEME_DIR=$(PWD)/theme
OUTPUT_DIR=$(PWD)/output
PDF_DIR=$(PWD)/pdf
PDF_GEN_TAG=astefanutti/decktape

default: run

pwd:
	@echo $(PWD)

img:
	@docker build -t $(TAG) .

run: img
	@docker run -it -p $(PORT):$(PORT) \
	-v $(CONTENT_DIR):/src/content \
	-v $(THEME_DIR):/src/assets \
	-v $(OUTPUT_DIR):/src/public \
	$(TAG) \
	server

build: img
	@docker run \
	-v $(CONTENT_DIR):/src/content \
	-v $(THEME_DIR):/src/assets \
	-v $(OUTPUT_DIR):/src/public \
	$(TAG)

pdf: build
	@docker run --rm -t -v $(PDF_DIR):/slides $(PDF_GEN_TAG) \
	$(OUTPUT_DIR)/index.html $(PDF_DIR)/presentation.pdf

help: img
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	help

new: build
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	new site $(PRES_NAME)

shell: build
	@docker run -it \
	-v $(CONTENT_DIR):/src/content \
	-v $(THEME_DIR):/src/assets \
	--entrypoint=bash $(TAG)

clean-all:
	@-docker volume rm $$(docker volume ls -qf dangling=true)
	@-docker system prune
