FROM node:current-alpine

RUN apk update && apk upgrade
RUN apk add yarn
RUN yarn global add @babel/core @babel/node @babel/preset-env \
    @babel/preset-typescript @exercism/babel-preset-javascript \
    @exercism/static-analysis @typescript-eslint/typescript-estree \
    @typescript-eslint/visitor-keys babel-jest chalk core-js jest typescript

RUN mkdir /javascript
WORKDIR /javascript

ENTRYPOINT ["sh", "-c"]
