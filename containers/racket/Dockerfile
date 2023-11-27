FROM racket/racket:latest

RUN raco pkg install --batch --auto main-distribution

RUN mkdir /racket
WORKDIR /racket
ENTRYPOINT ["sh", "-c"]
