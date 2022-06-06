FROM artifacts.leapyear.io/dockerhub-remote/ubuntu:20.04 AS builder

WORKDIR /build

# install stack
RUN apt-get update && apt-get install -y curl
ENV STACK_VERSION=2.7.5
RUN STACK_TAR_GZ="stack-${STACK_VERSION}-linux-x86_64.tar.gz" && \
    curl -sSLO "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/${STACK_TAR_GZ}" && \
    mkdir -p /opt/stack && \
    tar xvf "${STACK_TAR_GZ}" -C /opt/stack --strip-components=1 && \
    ln -sf /opt/stack/stack /usr/local/bin/ && \
    rm -rf "${STACK_TAR_GZ}"

# setup stack + GHC + third party dependencies
COPY \
    stack.yaml \
    stack.yaml.lock \
    hpack.yaml \
    ./
COPY mergit/package.yaml ./mergit/
COPY mergit-core/package.yaml ./mergit-core/
COPY github-schemas/package.yaml ./github-schemas/
COPY servant-github-app/package.yaml ./servant-github-app/
RUN apt-get update && \
    apt-get install -y \
        g++ \
        libgmp-dev \
        make \
        xz-utils \
        zlib1g-dev
RUN stack setup
RUN stack build --only-dependencies

# build everything else
COPY mergit/ ./mergit/
COPY mergit-core/ ./mergit-core/
COPY github-schemas/ ./github-schemas/
COPY servant-github-app/ ./servant-github-app/
COPY assets/ ./assets/
RUN stack install --local-bin-path /build/bin/

FROM artifacts.leapyear.io/dockerhub-remote/ubuntu:20.04

ENV APP_ROOT=/home/mergit/

# these need to be set at runtime
ENV GITHUB_APP_ID=
ENV GITHUB_CLIENT_ID=
ENV GITHUB_CLIENT_SECRET=
ENV GITHUB_WEBHOOK_SECRET=
ENV GITHUB_PRIVATE_KEY=
ENV GITHUB_USER_AGENT=
ENV COOKIE_JWK=
ENV MERGIT_URL=

COPY --from=builder /build/bin/mergit "${APP_ROOT}/bin/"

RUN groupadd mergit && useradd -g mergit mergit
RUN chown -R mergit:mergit "${APP_ROOT}/"

USER mergit
WORKDIR "${APP_ROOT}"

ENTRYPOINT "${APP_ROOT}/bin/mergit"
