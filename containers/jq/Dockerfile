# Dockerfile to run jq test using bats
FROM alpine
RUN apk -U upgrade
RUN apk add -U bash bats jq

RUN mkdir /jq
WORKDIR /jq
ENTRYPOINT ["bash", "-c"]
