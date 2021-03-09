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
	@rm -rf theme
	@cp -r ../lambda-days-2021/theme .
	@rm -rf content/arch
	@cp -r ../lambda-days-2021/content/* content/
	@cd docker && rm -rf archetypes static themes
	@cp -r ../lambda-days-2021/docker/archetypes docker/
	@cp -r ../lambda-days-2021/docker/static docker/
	@cp -r ../lambda-days-2021/docker/themes docker/
	@find . -type f | egrep '#|.*\..*~' | xargs rm
	docker build -t $(TAG) .

run: img
	@docker run -it -p $(PORT):$(PORT) \
	-v $(CONTENT_DIR):/src/content \
	-v $(THEME_DIR):/src/assets \
	-v $(OUTPUT_DIR):/src/public \
	$(TAG) \
	server

build: img
	docker run \
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

new: img
	@docker run -it -p $(PORT):$(PORT) -v $(CONTENT_DIR):/src/content $(TAG) \
	new site $(PRES_NAME)

shell: img
	@docker run -it \
	-v $(CONTENT_DIR):/src/content \
	-v $(THEME_DIR):/src/assets \
	--entrypoint=bash $(TAG)

clean-all:
	@-docker volume rm $$(docker volume ls -qf dangling=true)
	@-docker system prune
