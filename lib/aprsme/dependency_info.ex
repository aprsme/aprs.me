defmodule Aprsme.DependencyInfo do
  @moduledoc """
  Provides information about dependencies, particularly for production builds.
  """

  @doc """
  Gets the SHA1 hash of the aprs library currently being used.
  In development: reads from vendor/aprs directory
  In production: uses the hash captured at build time
  """
  def get_aprs_library_sha do
    if Application.get_env(:aprsme, :env, :dev) == :prod do
      # In production, use the static hash captured at build time
      get_static_parser_hash()
    else
      get_aprs_sha_from_vendor()
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
    resp = Req.get!("https://api.github.com/repos/aprsme/aprs/commits/main")
    body = resp.body
    sha = body["sha"] || body |> Jason.decode!() |> Access.get("sha")
    String.slice(sha, 0, 7)
  rescue
    _ -> nil
  end

  def is_latest_aprs_library? do
    get_aprs_library_sha() == latest_aprs_library_sha()
  end

  # Get static parser hash at compile time
  defp get_static_parser_hash do
    case System.get_env("PARSER_GIT_HASH") do
      nil ->
        "unknown"

      "unknown" ->
        # Try to read from the file copied during build
        hash_file = Path.join(["/app", "parser_hash.txt"])

        # Try alternative paths
        alternative_paths = [
          hash_file,
          Path.join([File.cwd!(), "parser_hash.txt"]),
          "parser_hash.txt"
        ]

        Enum.find_value(alternative_paths, "unknown", fn path ->
          if File.exists?(path) do
            case File.read(path) do
              {:ok, hash} -> String.trim(hash)
              _ -> nil
            end
          end
        end)

      hash ->
        hash
    end
  end
end
