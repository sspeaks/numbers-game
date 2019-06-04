FROM ubuntu

RUN apt-get update
RUN apt-get install curl libicu-dev libtinfo-dev libgmp-dev -y
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN git clone https://github.com/haskell/haskell-ide-engine --recursive
RUN cd haskell-ide-engine && stack ./install.hs hie-8.6.5 && stack ./install.hs build-doc

# Clean up
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*
ENV DEBIAN_FRONTEND=dialog

# Set the default shell to bash rather than sh
ENV SHELL /bin/bash
ENV PATH="/root/.local/bin:${PATH}"
