# syntax=docker/dockerfile:1.4

# Build arguments
ARG ELIXIR_VERSION=1.18.4
ARG OTP_VERSION=27.2.4
ARG DEBIAN_VERSION=bullseye-20250520-slim
ARG APP_NAME=aprs

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="debian:${DEBIAN_VERSION}"

# Platform args for multi-platform builds
ARG TARGETPLATFORM
ARG BUILDPLATFORM

# Stage 1: Dependencies only (cached layer)
FROM ${BUILDER_IMAGE} AS deps

# Install build dependencies
ENV DEBIAN_FRONTEND=noninteractive
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update -y && apt-get install -y --no-install-recommends \
    gcc \
    g++ \
    make \
    git

WORKDIR /app

# Install Hex and Rebar with cache mount
RUN --mount=type=cache,target=/root/.cache/rebar3,sharing=locked \
    --mount=type=cache,target=/root/.hex,sharing=locked \
    mix local.hex --force && \
    mix local.rebar --force && \
    mix archive.install hex mix_gleam 0.6.2 --force

ENV MIX_ENV=prod

# Copy only files needed for dependencies
COPY mix.exs mix.lock ./
COPY vendor vendor

# Get and compile dependencies with cache mount
RUN --mount=type=cache,target=/app/deps,sharing=locked \
    --mount=type=cache,target=/app/_build,sharing=locked \
    mix deps.get --only $MIX_ENV && \
    mix deps.compile && \
    cd vendor/aprs && \
    mix compile && \
    cd ../.. && \
    # Copy compiled deps to a location that persists
    cp -r deps /app/deps_compiled && \
    cp -r _build /app/_build_compiled

# Stage 2: Build application
FROM deps AS builder

# Copy pre-compiled dependencies
RUN cp -r /app/deps_compiled deps && \
    cp -r /app/_build_compiled _build

# Copy application code (ordered by change frequency)
COPY rel rel
COPY config config
COPY priv priv
COPY assets assets
COPY lib lib

# Build everything in one RUN with proper error handling
RUN --mount=type=cache,target=/root/.cache,sharing=locked <<EOF
    set -e
    
    # Setup BEAM files
    mkdir -p _build/prod/lib/aprs/ebin
    cp -r vendor/aprs/_build/prod/lib/aprs/ebin/* _build/prod/lib/aprs/ebin/
    
    mkdir -p _build/prod/lib/aprsme/ebin
    if [ -d "priv/gleam" ] && [ "$(ls -A priv/gleam/*.beam 2>/dev/null)" ]; then
        cp priv/gleam/*.beam _build/prod/lib/aprsme/ebin/
    fi
    
    # Compile and build release
    mix compile
    mix assets.deploy
    mix release --path /app/release
EOF

# Stage 3: Security scan (optional, can be commented out for faster builds)
FROM aquasec/trivy:latest AS security-scan
COPY --from=builder /app/release /scan
RUN trivy filesystem --exit-code 0 --no-progress --security-checks vuln /scan

# Stage 4: Create minimal runtime image
FROM ${RUNNER_IMAGE} AS runtime

# Create user first to avoid running as root
RUN useradd -r -u 1001 -g root -s /bin/false elixir

# Install only essential runtime dependencies in one layer
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked <<EOF
    set -e
    apt-get update -y
    apt-get install -y --no-install-recommends \
        libstdc++6 \
        openssl \
        libncurses5 \
        locales \
        ca-certificates
    
    # Generate locale
    sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen
    locale-gen
    
    # Clean up
    apt-get clean
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
    
    # Remove unnecessary files
    rm -rf /usr/share/doc /usr/share/man /usr/share/info /usr/share/locale/*
    
    # Remove package manager files we don't need
    rm -rf /var/log/dpkg.log /var/log/alternatives.log /var/log/apt
EOF

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

# Create app directory with correct permissions
RUN mkdir -p /app && chown -R elixir:root /app
WORKDIR /app

# Copy release with correct ownership
COPY --from=builder --chown=elixir:root /app/release ./

# Set deployment timestamp as elixir user
USER elixir
RUN date -u +"%Y-%m-%dT%H:%M:%SZ" > /app/deployed_at.txt

# Use exec form to ensure proper signal handling
ENTRYPOINT ["/app/bin/server"]

# Add metadata labels
LABEL maintainer="aprs.me" \
      security.scan="true" \
      security.user="non-root" \
      org.opencontainers.image.title="APRS.me" \
      org.opencontainers.image.description="Real-time APRS packet tracker" \
      org.opencontainers.image.vendor="aprs.me" \
      org.opencontainers.image.source="https://github.com/aprsme/aprs.me"