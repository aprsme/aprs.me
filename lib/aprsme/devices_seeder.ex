defmodule Aprsme.DevicesSeeder do
  @moduledoc false

  alias Aprsme.Devices
  alias Aprsme.Repo

  def seed_from_json(path \\ "test/support/test_devices.json") do
    {:ok, body} = File.read(path)
    {:ok, json} = Jason.decode(body)
    tocalls = Map.get(json, "tocalls", %{})
    mice = Map.get(json, "mice", %{})
    micelegacy = Map.get(json, "micelegacy", %{})
    now = DateTime.utc_now()

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
      end)
    end)
  end
end
