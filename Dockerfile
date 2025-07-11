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
RUN apt-get update -y && apt-get install -y --no-install-recommends build-essential git curl \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Prepare build directory
WORKDIR /app

# Install Hex and Rebar
RUN mix local.hex --force && mix local.rebar --force

# Set build environment
ENV MIX_ENV="prod"

# Install dependencies
COPY mix.exs mix.lock ./
# Copy vendor directory for local dependencies
COPY vendor vendor
RUN mix deps.get --only $MIX_ENV
# Extract the APRS parser hash from the vendored directory
RUN cd vendor/aprs && git rev-parse HEAD | cut -c1-7 > /tmp/aprs_hash.txt
RUN mix deps.compile

# Copy application code
COPY config config
COPY priv priv
COPY lib lib
COPY assets assets
COPY rel rel

# Compile assets
RUN mix assets.deploy

# Compile and release
RUN mix release --path /app/release

# Stage 2: Runtime
FROM ${RUNNER_IMAGE}

# Install runtime dependencies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends libstdc++6 openssl libncurses5 locales ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG="en_US.UTF-8" LANGUAGE="en_US:en" LC_ALL="en_US.UTF-8"

# Set working directory and create it
WORKDIR "/app"
RUN mkdir -p /app

# Set deployment timestamp to current time during runtime container build
RUN date -u +"%Y-%m-%dT%H:%M:%SZ" > /app/deployed_at.txt

# Copy APRS parser hash from builder stage
COPY --from=builder /tmp/aprs_hash.txt /app/aprs_hash.txt

# Set APRS parser hash environment variable for runtime
RUN APRS_HASH=$(cat /app/aprs_hash.txt) && echo "APRS_PARSER_HASH=$APRS_HASH" >> /etc/environment

# Copy release from builder
COPY --from=builder --chown=nobody:root /app/release ./

# Set ownership for the entire app directory
RUN chown -R nobody:root /app

# Set user and command
USER nobody
CMD ["/app/bin/server"]

# Optional: Add healthcheck
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:4000/health || exit 1
