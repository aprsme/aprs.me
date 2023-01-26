defmodule Aprs.Presence do
  use Phoenix.Presence, otp_app: :aprs, pubsub_server: Aprs.PubSub
end
