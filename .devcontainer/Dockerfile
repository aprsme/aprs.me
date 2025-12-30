FROM hexpm/elixir:1.17.3-erlang-27.2-debian-bullseye-20241202

# Install dev tools and PostgreSQL client
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    inotify-tools \
    postgresql-client \
    curl \
    wget \
    sudo \
    locales \
    && rm -rf /var/lib/apt/lists/*

# Generate locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen en_US.UTF-8

# Create non-root user
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Install hex and rebar
USER $USERNAME
RUN mix local.hex --force && \
    mix local.rebar --force

# Set environment
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV ELIXIR_ERL_OPTIONS="+fnu"

WORKDIR /workspace

# Create workspace directory with proper permissions
RUN mkdir -p /workspace && chown -R $USERNAME:$USERNAME /workspace

# Keep container running
CMD ["sleep", "infinity"]