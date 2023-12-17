defmodule Aprs.Presence do
  @moduledoc false
  use Phoenix.Presence, otp_app: :aprs, pubsub_server: Aprs.PubSub
end
