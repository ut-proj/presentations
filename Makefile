
lambda-days-2021/output:
	rm -rf published/lambda-days-2021
	cd lambda-days-2021 && \
	$(MAKE) build && \
	mv output ../published/lambda-days-2021
