FROM racket/racket:latest

# RUN raco pkg install --batch --auto main-distribution
RUN apt-get install -y jq
RUN raco pkg install zo-lib testing-util-lib rackunit-lib scheme-lib compiler-lib

RUN mkdir /racket
WORKDIR /racket
ENTRYPOINT ["sh", "-c"]
