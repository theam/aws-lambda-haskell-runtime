all:
	@rm -rf ./build/*
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t example-lambda .
	id=$$(docker create example-lambda); docker cp $$id:/root/output ./build; docker rm -v $$id
	cd build/output; zip -r function.zip *
