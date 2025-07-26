# Build arguments
ARG ELIXIR_VERSION=1.18.4
ARG OTP_VERSION=27.2.4
ARG DEBIAN_VERSION=bullseye-20250520-slim
ARG APP_NAME=aprs

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="debian:${DEBIAN_VERSION}"

# Stage 1: Build Elixir release
FROM ${BUILDER_IMAGE} AS builder

# Install build dependencies
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install -y --no-install-recommends \
    gcc \
    g++ \
    make \
    git \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Prepare build directory
WORKDIR /app

# Install Hex and Rebar
RUN mix local.hex --force && mix local.rebar --force

# Install mix_gleam archive (required for Gleam compilation)
RUN mix archive.install hex mix_gleam 0.6.2 --force

# Set build environment
ENV MIX_ENV="prod"

# Install dependencies
COPY mix.exs mix.lock ./
# Copy vendor directory for local dependencies
COPY vendor vendor
RUN mix deps.get --only $MIX_ENV

# Copy all application code
COPY config config
COPY lib lib
COPY assets assets
COPY priv priv
COPY rel rel

# Compile all dependencies
RUN mix deps.compile

# Manually compile the vendored aprs dependency
RUN cd vendor/aprs && \
    MIX_ENV=prod mix compile && \
    mkdir -p ../../_build/prod/lib/aprs/ebin && \
    cp -r _build/prod/lib/aprs/ebin/* ../../_build/prod/lib/aprs/ebin/ && \
    cd ../..

# Copy Gleam BEAM files before compiling the main app
RUN mkdir -p _build/prod/lib/aprsme/ebin && \
    cp priv/gleam/*.beam _build/prod/lib/aprsme/ebin/ || true

# Now compile the main application
RUN mix compile

# Compile assets using ESBuild and Tailwind (no Node.js needed)
RUN mix assets.deploy

# Compile and release
RUN mix release --path /app/release

# Stage 2: Runtime
FROM ${RUNNER_IMAGE}

# Install runtime dependencies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    libstdc++6 \
    openssl \
    libncurses5 \
    locales \
    ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG="en_US.UTF-8" LANGUAGE="en_US:en" LC_ALL="en_US.UTF-8"

# Set working directory
WORKDIR "/app"

# Set deployment timestamp to current time during runtime container build
RUN date -u +"%Y-%m-%dT%H:%M:%SZ" > /app/deployed_at.txt

# Copy release from builder with correct ownership
COPY --from=builder --chown=nobody:root /app/release ./

# Set user and command
USER nobody
CMD ["/app/bin/server"]
