FROM alpine
RUN apk add make
RUN apk add --repository="http://dl-cdn.alpinelinux.org/alpine/edge/testing" roswell
RUN ros install sbcl-bin
RUN ros install fiveam

RUN mkdir common-lisp
WORKDIR /common-lisp

ENTRYPOINT ["sh", "-c"]
