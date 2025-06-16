defmodule Aprs.PostgresTypes do
  @moduledoc false
end

Postgrex.Types.define(
  Aprs.PostgresTypes,
  [Geo.PostGIS.Extension] ++ Ecto.Adapters.Postgres.extensions(),
  json: Jason
)
