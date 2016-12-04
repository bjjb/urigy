FROM ubuntu:xenial

MAINTAINER JJ Buckley <jj@bjjb.org> (https://bjjb.org)

RUN apt-get update -y
RUN apt-get install -y clisp

RUN useradd -rm urigy
USER urigy
WORKDIR /home/urigy

COPY urigy.lisp .
EXPOSE 10923

CMD ["clisp", "urigy.lisp", "serve"]
