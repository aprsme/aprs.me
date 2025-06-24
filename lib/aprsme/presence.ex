defmodule Aprsme.Presence do
  @moduledoc false
  use Phoenix.Presence, otp_app: :aprsme, pubsub_server: Aprsme.PubSub
end
