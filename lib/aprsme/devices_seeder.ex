defmodule Aprsme.DevicesSeeder do
  @moduledoc false

  alias Aprsme.Devices
  alias Aprsme.Repo

  def seed_from_json(path \\ "test/support/test_devices.json") do
    case File.read(path) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, json} ->
            seed_from_decoded_json(json)

          {:error, reason} ->
            {:error, "Failed to decode JSON: #{inspect(reason)}"}
        end

      {:error, reason} ->
        {:error, "Failed to read file #{path}: #{inspect(reason)}"}
    end
  end

  defp seed_from_decoded_json(json) do
    tocalls = Map.get(json, "tocalls", %{})
    mice = Map.get(json, "mice", %{})
    micelegacy = Map.get(json, "micelegacy", %{})
    now = DateTime.utc_now()

    Repo.delete_all(Devices)

    Enum.each([tocalls, mice, micelegacy], fn group ->
      seed_device_group(group, now)
    end)

    {:ok, :seeded}
  end

  defp seed_device_group(group, now) do
    Enum.each(group, fn {identifier, attrs} ->
      insert_device(identifier, attrs, now)
    end)
  end

  defp insert_device(identifier, attrs, now) do
    attrs =
      attrs
      |> Map.put("identifier", identifier)
      |> Map.update("features", nil, fn f ->
        if is_list(f), do: f, else: [f]
      end)
      |> Map.put("updated_at", now)

    %Devices{}
    |> Devices.changeset(attrs)
    |> Ecto.Changeset.apply_changes()
    |> Map.from_struct()
    |> then(fn map ->
      Repo.insert!(
        Devices.changeset(%Devices{}, map),
        on_conflict: :replace_all,
        conflict_target: :identifier
      )
    end)
  end
end
