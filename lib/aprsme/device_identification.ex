defmodule Aprsme.DeviceIdentification do
  @moduledoc """
  Handles APRS device identification based on the APRS device identification database.
  """

  import Ecto.Query

  alias Aprsme.Devices
  alias Aprsme.Repo

  @device_patterns [
    {~r/^ \x00\x00$/, "Original MIC-E"},
    {~r/^>\x00\^$/, "Kenwood TH-D74"},
    {~r/^>\x00\x00$/, "Kenwood TH-D74A"},
    {~r/^]\x00=$/, "Kenwood DM-710"},
    {~r/^]\x00\x00$/, "Kenwood DM-700"},
    {~r/^`_ $/, "Yaesu VX-8"},
    {~r/^`_\"$/, "Yaesu FTM-350"},
    {~r/^`_#$/, "Yaesu VX-8G"},
    {~r/^`_\$$/, "Yaesu FT1D"},
    {~r/^`_%$/, "Yaesu FTM-400DR"},
    {~r/^`_\)$/, "Yaesu FTM-100D"},
    {~r/^`_\($/, "Yaesu FT2D"},
    {~r/^` X$/, "AP510"},
    {~r/^`\x00\x00$/, "Mic-Emsg"},
    {~r/^'\|3$/, "Byonics TinyTrack3"},
    {~r/^'\|4$/, "Byonics TinyTrack4"},
    {~r/^':4$/, "SCS GmbH & Co. P4dragon DR-7400 modems"},
    {~r/^':8$/, "SCS GmbH & Co. P4dragon DR-7800 modems"},
    {~r/^'\x00\x00$/, "McTrackr"},
    {~r/^\x00\"\x00$/, "Hamhud"},
    {~r/^\x00\/\x00$/, "Argent"},
    {~r/^\x00\^\x00$/, "HinzTec anyfrog"},
    {~r/^\x00\*\x00$/, "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"},
    {~r/^\x00~\x00$/, "Other"}
  ]

  @doc """
  Identifies the manufacturer and model of an APRS device based on its symbol pattern.
  Returns a tuple of {manufacturer, model} or "Unknown" if the device cannot be identified.

  ## Examples

      iex> Aprsme.DeviceIdentification.identify_device(">" <> <<0>> <> "^")
      "Kenwood TH-D74"

      iex> Aprsme.DeviceIdentification.identify_device("`_#")
      "Yaesu VX-8G"

      iex> Aprsme.DeviceIdentification.identify_device("not-a-match")
      "Unknown"
  """
  @spec identify_device(String.t()) :: String.t()
  def identify_device(symbols) do
    Enum.find_value(@device_patterns, "Unknown", fn {regex, name} ->
      if Regex.match?(regex, symbols) do
        name
      end
    end)
  end

  @doc """
  Returns a list of all known device manufacturers.

  ## Examples

      iex> "Kenwood" in Aprsme.DeviceIdentification.known_manufacturers()
      true

      iex> Enum.member?(Aprsme.DeviceIdentification.known_manufacturers(), "AP510")
      true
  """
  @spec known_manufacturers() :: [String.t()]
  def known_manufacturers do
    [
      "Original MIC-E",
      "Kenwood",
      "Yaesu",
      "AP510",
      "Byonics",
      "SCS GmbH & Co.",
      "McTrackr",
      "Hamhud",
      "Argent",
      "HinzTec",
      "APOZxx",
      "Other"
    ]
  end

  @doc """
  Returns a list of all known device models for a given manufacturer.

  ## Examples

      iex> Aprsme.DeviceIdentification.known_models("Kenwood")
      ["TH-D74", "TH-D74A", "DM-710", "DM-700"]

      iex> Aprsme.DeviceIdentification.known_models("Unknown")
      []
  """
  @spec known_models(String.t()) :: [String.t()]
  def known_models("Kenwood"), do: ["TH-D74", "TH-D74A", "DM-710", "DM-700"]

  def known_models("Yaesu"), do: ["VX-8", "FTM-350", "VX-8G", "FT1D", "FTM-400DR", "FTM-100D", "FT2D"]

  def known_models("Byonics"), do: ["TinyTrack3", "TinyTrack4"]
  def known_models("SCS GmbH & Co."), do: ["P4dragon DR-7400 modems", "P4dragon DR-7800 modems"]
  def known_models(_), do: []

  @url "https://aprs-deviceid.aprsfoundation.org/tocalls.dense.json"
  @week_seconds 7 * 24 * 60 * 60

  def maybe_refresh_devices do
    last = Repo.one(from d in Devices, order_by: [desc: d.updated_at], limit: 1)

    last_time =
      case last && last.updated_at do
        %NaiveDateTime{} = naive -> DateTime.from_naive!(naive, "Etc/UTC")
        %DateTime{} = dt -> dt
        _ -> nil
      end

    if last == nil or (last_time && DateTime.diff(DateTime.utc_now(), last_time) > @week_seconds) do
      fetch_and_upsert_devices()
    else
      :ok
    end
  end

  def fetch_and_upsert_devices do
    req = Finch.build(:get, @url)

    case Finch.request(req, Aprsme.Finch) do
      {:ok, %Finch.Response{status: 200, body: body}} ->
        {:ok, json} = Jason.decode(body)
        upsert_devices(json)

      err ->
        err
    end
  end

  def upsert_devices(json) do
    tocalls = Map.get(json, "tocalls", %{})
    mice = Map.get(json, "mice", %{})
    micelegacy = Map.get(json, "micelegacy", %{})
    now = DateTime.utc_now()

    Repo.transaction(fn ->
      Repo.delete_all(Devices)

      Enum.each([tocalls, mice, micelegacy], fn group ->
        Enum.each(group, fn {identifier, attrs} ->
          attrs =
            attrs
            |> Map.put("identifier", identifier)
            |> Map.update("features", nil, fn f ->
              if is_list(f), do: f, else: [f]
            end)
            |> Map.put("updated_at", now)

          %Devices{} |> Devices.changeset(attrs) |> Repo.insert!()
        end)
      end)
    end)

    :ok
  end

  # Helper to enqueue the job
  def enqueue_refresh_job do
    Oban.insert!(Aprsme.DeviceIdentification.Worker.new(%{}))
  end

  @doc """
  Looks up a device by identifier, using ? as a single-character wildcard.
  Returns the device struct if found, or nil.

  Note: This function requires the devices table to be seeded and a running Repo context.
  """
  def lookup_device_by_identifier(nil), do: nil

  def lookup_device_by_identifier(identifier) when is_binary(identifier) do
    # Fetch all device patterns from DB
    devices = Repo.all(Devices)

    Enum.find(devices, fn device ->
      pattern = device.identifier

      cond do
        String.contains?(pattern, "?") ->
          try do
            regex = wildcard_pattern_to_regex(pattern)
            Regex.match?(regex, identifier)
          rescue
            _e in Regex.CompileError ->
              false
          end

        String.contains?(pattern, "*") ->
          # Compare literally if pattern contains * but not ?
          pattern == identifier

        true ->
          pattern == identifier
      end
    end)
  end

  # Converts a pattern with ? wildcards to a regex
  defp wildcard_pattern_to_regex(pattern) when is_binary(pattern) do
    # Replace ? with a placeholder, escape all regex metacharacters except the placeholder, then replace placeholder with .
    pattern
    |> String.replace("?", "__WILDCARD__")
    # Escape all regex metacharacters
    |> String.replace(~r/([\\.\+\*\?\[\^\]\$\(\)\{\}=!<>\|:\-])/, "\\\\\1")
    |> String.replace("__WILDCARD__", ".")
    |> then(&~r/^#{&1}$/)
  end
end

defmodule Aprsme.DeviceIdentification.Worker do
  @moduledoc false
  use Oban.Worker, queue: :default, max_attempts: 1

  @impl true
  def perform(_job) do
    Aprsme.DeviceIdentification.maybe_refresh_devices()
    :ok
  end
end
