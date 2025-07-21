defmodule Mix.Tasks.SetupGleam do
  @shortdoc "Installs mix_gleam archive if not already installed"

  @moduledoc false
  use Mix.Task

  def run(_) do
    if archive_installed?() do
      Mix.shell().info("mix_gleam archive already installed")
    else
      Mix.shell().info("Installing mix_gleam archive...")
      # Use System.cmd to avoid Mix dependency issues
      {_, 0} = System.cmd("mix", ["archive.install", "hex", "mix_gleam", "--force"])
    end
  end

  defp archive_installed? do
    archives_path = Path.join([Mix.Utils.mix_home(), "archives"])

    if File.exists?(archives_path) do
      archives_path
      |> File.ls!()
      |> Enum.any?(&String.starts_with?(&1, "mix_gleam"))
    else
      false
    end
  end
end
