# Build arguments
ARG ELIXIR_VERSION=1.18.4
ARG OTP_VERSION=27.2.4
ARG DEBIAN_VERSION=bullseye-20250520-slim

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="debian:${DEBIAN_VERSION}"

# Build stage
FROM ${BUILDER_IMAGE} AS builder

# Install build dependencies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends gcc g++ make git && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install hex + rebar
RUN mix local.hex --force && \
    mix local.rebar --force

ENV MIX_ENV=prod

# Install mix dependencies
COPY mix.exs mix.lock ./
RUN mix deps.get --only $MIX_ENV && \
    mix deps.compile

# Copy and compile application
COPY config config
COPY lib lib
COPY assets assets
COPY priv priv
COPY rel rel

# Build application
RUN mix compile && \
    mix assets.deploy && \
    mix release

# Runtime stage
FROM ${RUNNER_IMAGE}

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    libstdc++6 openssl libncurses5 locales ca-certificates && \
    sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen && \
    apt-get clean && rm -rf /var/lib/apt/lists/* && \
    useradd -r -u 1001 -g root -s /bin/false elixir

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

WORKDIR /app

# Ensure proper ownership
RUN chown -R elixir:root /app

COPY --from=builder --chown=elixir:root /app/_build/prod/rel/aprsme ./

USER elixir
CMD ["/app/bin/server"]