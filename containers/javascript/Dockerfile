FROM node:current-alpine

RUN apk add yarn
RUN yarn global add core-js babel-jest jest chalk

RUN mkdir /javascript
WORKDIR /javascript

ENTRYPOINT ["sh", "-c"]
