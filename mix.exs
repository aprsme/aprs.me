defmodule Aprs.MixProject do
  use Mix.Project

  def project do
    [
      app: :aprs,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      releases: releases(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Aprs.Application, []},
      extra_applications: [:hackney, :logger, :runtime_tools, :os_mon]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:bcrypt_elixir, "~> 3.0"},
      {:certifi, "~> 2.9"},
      {:ecto_psql_extras, "~> 0.8.8"},
      {:ecto_sql, "~> 3.11"},
      {:finch, "~> 0.13"},
      {:geo, "~> 4.0.1"},
      {:geo_postgis, "~> 3.4"},
      {:geocalc, "~> 0.8"},
      {:gettext, "~> 0.26.2"},
      {:hackney, "~> 1.24"},
      {:heroicons, "~> 0.5"},
      {:jason, "~> 1.2"},
      {:libcluster, "~> 3.3"},
      {:oban, "~> 2.11"},
      {:phoenix, "~> 1.7.0-rc.2", override: true},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_html, "~> 4.0"},
      {:phoenix_live_dashboard, "~> 0.8"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 1.0.2"},
      {:plug_cowboy, "~> 2.5"},
      {:postgrex, ">= 0.0.0"},
      {:swoosh, "~> 1.3"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:esbuild, "~> 0.5", runtime: Mix.env() == :dev, config: "assets/esbuild.config.js"},
      {:tailwind, "~> 0.3.1", runtime: Mix.env() == :dev},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false},
      {:excoveralls, "~> 0.18", only: :test, runtime: false},
      {:exvcr, "~> 0.15", only: :test},
      {:faker, "~> 0.18", only: [:dev, :test]},
      {:floki, ">= 0.30.0", only: :test},
      {:mix_test_watch, "~> 1.1", only: [:dev, :test]},
      {:sobelow, "~> 0.8", only: :dev},
      {:stream_data, "~> 1.2.0", only: [:dev, :test]},
      {:styler, "~> 1.4.2", only: [:dev, :test], runtime: false}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end

  defp releases do
    [
      aprs: [
        cookie: "aprs"
      ]
    ]
  end
end
