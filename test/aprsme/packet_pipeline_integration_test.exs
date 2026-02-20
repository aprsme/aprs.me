defmodule Aprsme.PacketPipelineIntegrationTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.Performance.InsertOptimizer

  describe "packet pipeline under load" do
    test "insert optimizer provides reasonable batch sizes" do
      batch_size = InsertOptimizer.get_optimal_batch_size()
      assert batch_size >= 100
      assert batch_size <= 800
    end
  end
end
