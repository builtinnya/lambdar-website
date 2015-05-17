## Pull base image
FROM dockerfile/ubuntu

## Ensure locale is set during build
ENV LANG C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends cabal-install-1.20 ghc-7.8.4 \
            happy-1.19.4 alex-3.1.3 zlib1g-dev libtinfo-dev libsqlite3-0 \
            libsqlite3-dev git-core build-essential zlib1g-dev && \
    rm -rf /var/lib/apt/lists/* && \
    # Clone lambdar repo
    git clone https://github.com/builtinnya/lambdar-website

ENV PATH /root/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH

WORKDIR lambdar-website

RUN cabal update && \
    # Install yesod binaries
    cabal install alex happy yesod-bin && \
    # Install dependencies
    cabal install
    # Build for production
    yesod keter

## Run bash by default unless a command is specified
CMD ["bash"]
