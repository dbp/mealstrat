.PHONY: deploy
deploy:
	sed -e "s/HASH/`git rev-parse HEAD`/g" deploy/Dockerrun.aws.json.template > deploy/Dockerrun.aws.json
	cd deploy; zip artifact.zip Dockerrun.aws.json
	eb deploy
	rm deploy/artifact.zip; rm deploy/Dockerrun.aws.json
