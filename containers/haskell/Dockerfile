FROM haskell:slim
RUN mkdir /haskell

RUN stack upgrade
RUN stack setup --install-ghc --resolver lts-20.18

WORKDIR /haskell
ENTRYPOINT ["sh", "-c"]
