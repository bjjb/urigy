build:
	docker build -t bjjb/urigy .

release: build
	docker push bjjb/urigy
