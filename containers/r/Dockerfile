FROM r-base

RUN Rscript -e 'install.packages(c("devtools", "roxygen2", "testthat", "knitr"))'
RUN mkdir /r

WORKDIR /r
ENTRYPOINT ["sh", "-c"]
