#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:2f4b2c4b44ec6d9f4afc9578d8475f7272dc50783268dc672d19b0db6098b89d

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        hunspell \
        hunspell-cs \
        hunspell-de-de \
        hunspell-en-gb \
        hunspell-en-us \
        hunspell-es \
        hunspell-fr \
        hunspell-nl \
        myspell-et && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/bin && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
COPY bin/ ${HOME}/bin/
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}

ENV PATH=${HOME}/bin:$PATH \
    LANG=C.UTF-8

CMD ["isx-plug-spellchecker", "-b", "localhost", "-p", "8000"]

EXPOSE 8000

HEALTHCHECK CMD curl -fs http://localhost:8000 || false
#===============================================================================
