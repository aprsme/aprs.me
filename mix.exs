defmodule Aprsme.MixProject do
  use Mix.Project

  def project do
    [
      app: :aprsme,
      version: "0.2.0",
      elixir: "~> 1.17",
      archives: [mix_gleam: "~> 0.6"],
      compilers: Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      erlc_include_path: "build/#{Mix.env()}/erlang/aprsme/include",
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      listeners: [Phoenix.CodeReloader],
      dialyzer: [
        ignore_warnings: ".dialyzer_ignore.exs",
        plt_file:
          {:no_warn,
           "_build/dev/dialyxir_erlang-#{:erlang.system_info(:otp_release)}_elixir-#{System.version()}_deps-dev.plt"}
      ]
    ]
  end

  def cli do
    [
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
      mod: {Aprsme.Application, []},
      extra_applications: [:hackney, :logger, :runtime_tools, :os_mon]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies erlc paths per environment for Gleam compilation
  defp erlc_paths(env) do
    ["build/#{env}/erlang/aprsme/_gleam_artefacts", "src"]
  end

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:error_tracker, "~> 0.6"},
      {:bcrypt_elixir, "~> 3.0"},
      {:certifi, "~> 2.9"},
      {:ecto_psql_extras, "~> 0.8.8"},
      {:ecto_sql, "~> 3.11"},
      {:geo, "~> 4.0.1"},
      {:geo_postgis, "~> 3.4"},
      {:geocalc, "~> 0.8"},
      {:gen_stage, "~> 1.2"},
      {:gettext, "~> 0.26.2"},
      {:hackney, "~> 1.24"},
      {:jason, "~> 1.4"},
      {:libcluster, "~> 3.3"},
      {:oban, "~> 2.11"},
      {:phoenix, "~> 1.8.0-rc.3", override: true},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_html, "~> 4.0"},
      {:phoenix_live_dashboard, "~> 0.8"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 1.0.17"},
      {:postgrex, ">= 0.0.0"},
      {:swoosh, "~> 1.16"},
      {:resend, "~> 0.4.1"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      aprs_dep(),
      {:esbuild, "~> 0.9", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.3", runtime: Mix.env() == :dev},
      {:heroicons,
       github: "tailwindlabs/heroicons", tag: "v2.1.1", sparse: "optimized", app: false, compile: false, depth: 1},
      {:bandit, "~> 1.5"},
      {:req, "~> 0.5"},
      {:gridsquare, "~> 0.2.0"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0", only: :dev, runtime: false},
      {:exvcr, "~> 0.15", only: :test},
      {:faker, "~> 0.18", only: [:dev, :test]},
      {:floki, ">= 0.30.0", only: :test},
      {:mix_test_watch, "~> 1.1", only: [:dev, :test]},
      {:sobelow, "~> 0.8", only: :dev},
      {:stream_data, "~> 1.2.0", only: [:dev, :test]},
      {:igniter, "~> 0.6", only: [:dev, :test]},
      {:mox, "~> 1.2", only: :test},
      {:styler, "~> 1.5.0", only: [:dev, :test], runtime: false},
      {:httpoison, "~> 1.8"},
      {:hammer, "~> 7.0"},
      {:cachex, "~> 4.1"},
      {:gettext_pseudolocalize, "~> 0.1"},
      # Gleam dependencies
      {:gleam_stdlib, ">= 0.60.0 and < 1.0.0", app: false, override: true},
      {:gleeunit, "~> 1.0", only: [:dev, :test], runtime: false, app: false}
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
      setup: ["deps.get", "ecto.setup", "gleam_compile"],
      compile: ["gleam_compile", "compile"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["gleam_compile", "ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.deploy": [
        "tailwind default --minify",
        "esbuild default --minify",
        "phx.digest"
      ]
    ]
  end

  defp aprs_dep do
    {:aprs, path: "vendor/aprs"}
    # if Mix.env() in [:dev] do
    #   {:aprs, path: "vendor/aprs"}
    # else
    #   {:aprs, github: "aprsme/aprs", branch: "main"}
    # end
  end

end
