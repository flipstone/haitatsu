FROM ubuntu:trusty-20150320

RUN apt-get update &&\
    apt-get install -y --no-install-recommends \
            ca-certificates libgmp10 &&\
    apt-get clean

WORKDIR /deploy
ENTRYPOINT ["/opt/haitatsu/haitatsu"]
ADD haitatsu /opt/haitatsu/haitatsu
