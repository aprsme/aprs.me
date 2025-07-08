defmodule Aprsme.DependencyInfo do
  @moduledoc """
  Provides information about dependencies, particularly for production builds.
  """

  alias Aprsme.CircuitBreaker

  @doc """
  Gets the SHA1 hash of the aprs library currently being used.
  In development: reads from vendor/aprs directory
  In production: uses the hash captured at compile time
  """
  def get_aprs_library_sha do
    # First try environment variable (production)
    case System.get_env("APRS_PARSER_HASH") do
      nil -> get_aprs_sha_from_vendor()
      "unknown" -> get_aprs_sha_from_vendor()
      hash -> hash
    end
  end

  defp get_aprs_sha_from_vendor do
    vendor_path = Path.join([File.cwd!(), "vendor", "aprs"])

    if File.dir?(vendor_path) do
      case System.cmd("git", ["rev-parse", "HEAD"], cd: vendor_path) do
        {sha, 0} -> String.slice(String.trim(sha), 0, 7)
        _ -> nil
      end
    end
  rescue
    _ -> nil
  end

  def latest_aprs_library_sha do
    case CircuitBreaker.call(
           :github_api,
           fn ->
             resp = Req.get!("https://api.github.com/repos/aprsme/aprs/commits/main")
             body = resp.body
             sha = body["sha"] || body |> Jason.decode!() |> Access.get("sha")
             String.slice(sha, 0, 7)
           end,
           10_000
         ) do
      {:ok, sha} -> sha
      {:error, _} -> nil
    end
  end

  def is_latest_aprs_library? do
    get_aprs_library_sha() == latest_aprs_library_sha()
  end
end
