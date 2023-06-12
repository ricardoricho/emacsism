FROM python:alpine
RUN python3 -m pip install pytest pytest-cache pytest-subtests pytest-pylint

RUN mkdir /python

WORKDIR /python
ENTRYPOINT ["sh", "-c"]
