# Aprs

## Prerequisites

Before setting up the project, ensure you have the following installed:
- Elixir 1.17+
- Erlang/OTP
- PostgreSQL with PostGIS extension

## Setup

To start your Phoenix server:

  * Run the complete setup:
    ```bash
    mix setup
    ```
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Deployment

This application is deployed to a Kubernetes cluster using Flux GitOps. The Kubernetes manifests are in the `k8s/` directory and are automatically applied by Flux when changes are pushed to the main branch.

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix
