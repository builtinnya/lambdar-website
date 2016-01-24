## Pull base image
FROM dockerfile/ubuntu

## Ensure locale is set during build
ENV LANG C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends cabal-install-1.22 ghc-7.10.3 \
            happy-1.19.5 alex-3.1.4 zlib1g-dev libtinfo-dev libsqlite3-0 \
            libsqlite3-dev git-core build-essential zlib1g-dev && \
    rm -rf /var/lib/apt/lists/* && \
    # Clone lambdar repo
    git clone https://github.com/builtinnya/lambdar-website

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH

WORKDIR $HOME

# Install yesod binaries
RUN cabal update && \
    cabal install yesod-bin

WORKDIR lambdar-website

RUN cabal sandbox init && \
    cabal update && \
    # Install dependencies
    cabal install && \
    # Build for production
    yesod keter

## Run bash by default unless a command is specified
CMD ["bash"]
