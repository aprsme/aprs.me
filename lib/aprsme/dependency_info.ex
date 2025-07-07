defmodule Aprsme.DependencyInfo do
  @moduledoc """
  Provides information about dependencies, particularly for production builds.
  """

  @aprs_hash (
               mix_lock_path = Path.join([File.cwd!(), "mix.lock"])

               if File.exists?(mix_lock_path) do
                 case File.read(mix_lock_path) do
                   {:ok, content} ->
                     case Regex.run(
                            ~r/aprs:\s*\{:git,\s*"https:\/\/github\.com\/aprsme\/aprs",\s*\{:ref,\s*"([a-f0-9]+)"\}/,
                            content
                          ) do
                       [_, hash] ->
                         String.slice(hash, 0, 7)

                       _ ->
                         case Regex.run(
                                ~r/aprs:\s*\{:git,\s*"https:\/\/github\.com\/aprsme\/aprs",\s*"([a-f0-9]+)"\}/,
                                content
                              ) do
                           [_, hash] -> String.slice(hash, 0, 7)
                           _ -> "unknown"
                         end
                     end

                   _ ->
                     "unknown"
                 end
               else
                 "unknown"
               end
             )

  @doc """
  Gets the SHA1 hash of the aprs library currently being used.
  In development: reads from vendor/aprs directory
  In production: uses the hash captured at compile time
  """
  def get_aprs_library_sha do
    if Application.get_env(:aprsme, :env, :dev) == :prod do
      @aprs_hash
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
end
