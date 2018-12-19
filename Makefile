deploy:
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/bootstrap .
	@zip runtime.zip bootstrap
	aws lambda publish-layer-version --layer-name haskell-runtime-dev --zip-file fileb://runtime.zip
	@rm bootstrap runtime.zip

# USAGE: make VERSION=42 publish
# Replace 42 with the version you are publishing
publish:
	aws lambda add-layer-version-permission --layer-name haskell-runtime-dev --version-number ${VERSION} --principal "*" --statement-id publish --action lambda:GetLayerVersion