ARG ELIXIR_VERSION=1.18.4
ARG OTP_VERSION=27.2.4
ARG DEBIAN_VERSION=bullseye-20250520-slim

# Set default non-root user and group IDs
ARG USER_ID=1000
ARG GROUP_ID=1000

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="debian:${DEBIAN_VERSION}"

FROM ${BUILDER_IMAGE} AS builder

# install build dependencies
RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y build-essential git && \
    apt-get clean && \
    rm -f /var/lib/apt/lists/*_*

# prepare build dir
WORKDIR /app

# install hex + rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# set build ENV
ENV MIX_ENV="prod"

# install mix dependencies
COPY mix.exs mix.lock ./
RUN mix deps.get --only $MIX_ENV
RUN mkdir config

# copy compile-time config files before we compile dependencies
# to ensure any relevant config change will trigger the dependencies
# to be re-compiled.
COPY config/config.exs config/${MIX_ENV}.exs config/
RUN mix deps.compile

COPY priv priv

COPY lib lib

COPY assets assets

# compile assets
RUN mix assets.deploy

# Compile the release
RUN mix compile

# Changes to config/runtime.exs don't require recompiling the code
COPY config/runtime.exs config/

COPY rel rel
RUN mix release

# start a new build stage so that the final image will only contain
# the compiled release and other runtime necessities
FROM ${RUNNER_IMAGE}

# Install security updates and required packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
    libstdc++6 \
    openssl \
    libncurses5 \
    locales \
    ca-certificates \
    tini && \
    apt-get clean && \
    rm -f /var/lib/apt/lists/*_* && \
    # Create a non-root user and group with specific ID
    groupadd -g ${GROUP_ID} aprs && \
    useradd -r -g aprs -u ${USER_ID} -s /bin/false -M aprs && \
    # Remove setuid and setgid permissions
    find / -perm /6000 -type f -exec chmod a-s {} \; || true && \
    # Secure system configurations
    chmod 0600 /etc/login.defs && \
    chmod 0600 /etc/passwd && \
    chmod 0600 /etc/group && \
    # Create and secure app directory
    mkdir -p /app && \
    chown aprs:aprs /app && \
    chmod 0750 /app

# Set the locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

WORKDIR "/app"

# Set security-related environment variables
ENV MIX_ENV="prod" \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8 \
    # Disable history files
    HISTFILE=/dev/null \
    # Prevent writing .erlang.cookie file
    HOME=/dev/null \
    # Add security headers
    SECURITY_HEADERS="true" \
    # Disable debug info in production
    ERL_AFLAGS="+S 1:1 +A 1 +K true -kernel shell_history enabled -kernel shell_history_file_bytes 0" \
    PHX_SERVER=true

# Only copy the final release from the build stage
COPY --from=builder --chown=${USER_ID}:${GROUP_ID} /app/_build/${MIX_ENV}/rel/aprs ./

USER ${USER_ID}

# Ensure the server binary is executable
RUN chmod +x /app/bin/server

# Use tini as init process to handle signals and zombie processes
ENTRYPOINT ["/usr/bin/tini", "--"]

# Add specific capabilities needed by the app
CMD ["/app/bin/server"]

# Add security-related metadata
LABEL org.opencontainers.image.vendor="APRS.me" \
    org.opencontainers.image.title="APRS.me Server" \
    org.opencontainers.image.description="APRS.me server with security hardening" \
    org.opencontainers.image.version="${MIX_ENV}" \
    org.opencontainers.image.created="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
    security.root-user="false" \
    security.non-root-user="app" \
    security.privileged="false"

# Container configuration is handled by Dokku
EXPOSE $PORT
