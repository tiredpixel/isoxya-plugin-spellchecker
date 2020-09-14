#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:2f53b5f0101a2f296c632c28010df43575b8ced96615b4b0b370d52a7004f473

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
COPY .version ${HOME}/.version
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}

ENV ADDRESS=localhost \
    LANG=C.UTF-8 \
    PATH=${HOME}/bin:$PATH \
    PORT=8000

CMD isx-plugin-spellchecker -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
