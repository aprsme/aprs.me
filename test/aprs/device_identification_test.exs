defmodule Aprs.DeviceIdentificationTest do
  use Aprs.DataCase, async: false

  alias Aprs.DeviceIdentification
  import DevicesSeeder

  describe "identify_device/1" do
    test "identifies Original MIC-E devices" do
      assert DeviceIdentification.identify_device(" " <> <<0, 0>>) == "Original MIC-E"
    end

    test "identifies Kenwood devices" do
      assert DeviceIdentification.identify_device(">" <> <<0>> <> "^") == "Kenwood TH-D74"
      assert DeviceIdentification.identify_device(">" <> <<0, 0>>) == "Kenwood TH-D74A"
      assert DeviceIdentification.identify_device("]" <> <<0>> <> "=") == "Kenwood DM-710"
      assert DeviceIdentification.identify_device("]" <> <<0, 0>>) == "Kenwood DM-700"
    end

    test "identifies Yaesu devices" do
      assert DeviceIdentification.identify_device("`_" <> " ") == "Yaesu VX-8"
      assert DeviceIdentification.identify_device("`_" <> "\"") == "Yaesu FTM-350"
      assert DeviceIdentification.identify_device("`_" <> "#") == "Yaesu VX-8G"
      assert DeviceIdentification.identify_device("`_" <> "$") == "Yaesu FT1D"
      assert DeviceIdentification.identify_device("`_" <> "%") == "Yaesu FTM-400DR"
      assert DeviceIdentification.identify_device("`_" <> ")") == "Yaesu FTM-100D"
      assert DeviceIdentification.identify_device("`_" <> "(") == "Yaesu FT2D"
    end

    test "identifies other devices" do
      assert DeviceIdentification.identify_device("` X") == "AP510"
      assert DeviceIdentification.identify_device("`" <> <<0, 0>>) == "Mic-Emsg"
      assert DeviceIdentification.identify_device("'|3") == "Byonics TinyTrack3"
      assert DeviceIdentification.identify_device("'|4") == "Byonics TinyTrack4"

      assert DeviceIdentification.identify_device("':4") ==
               "SCS GmbH & Co. P4dragon DR-7400 modems"

      assert DeviceIdentification.identify_device("':8") ==
               "SCS GmbH & Co. P4dragon DR-7800 modems"

      assert DeviceIdentification.identify_device("'" <> <<0, 0>>) == "McTrackr"
      assert DeviceIdentification.identify_device(<<0>> <> "\"" <> <<0>>) == "Hamhud"
      assert DeviceIdentification.identify_device(<<0>> <> "/" <> <<0>>) == "Argent"
      assert DeviceIdentification.identify_device(<<0>> <> "^" <> <<0>>) == "HinzTec anyfrog"

      assert DeviceIdentification.identify_device(<<0>> <> "*" <> <<0>>) ==
               "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"

      assert DeviceIdentification.identify_device(<<0>> <> "~" <> <<0>>) == "Other"
    end

    test "returns Unknown for unidentified devices" do
      assert DeviceIdentification.identify_device(<<0, 0, 0>>) == "Unknown"
    end
  end

  describe "known_manufacturers/0" do
    test "returns list of known manufacturers" do
      manufacturers = DeviceIdentification.known_manufacturers()
      assert is_list(manufacturers)
      assert "Kenwood" in manufacturers
      assert "Yaesu" in manufacturers
      assert "Byonics" in manufacturers
    end
  end

  describe "known_models/1" do
    test "returns list of known models for Kenwood" do
      models = DeviceIdentification.known_models("Kenwood")
      assert is_list(models)
      assert "TH-D74" in models
      assert "TH-D74A" in models
      assert "DM-710" in models
      assert "DM-700" in models
    end

    test "returns list of known models for Yaesu" do
      models = DeviceIdentification.known_models("Yaesu")
      assert is_list(models)
      assert "VX-8" in models
      assert "FTM-350" in models
      assert "VX-8G" in models
      assert "FT1D" in models
      assert "FTM-400DR" in models
      assert "FTM-100D" in models
      assert "FT2D" in models
    end

    test "returns empty list for unknown manufacturer" do
      assert DeviceIdentification.known_models("Unknown") == []
    end
  end

  describe "lookup_device_by_identifier/1" do
    test "matches APSK21 to APS??? pattern" do
      # Seed the devices table from the JSON
      seed_from_json()

      # Should match
      found = Aprs.DeviceIdentification.lookup_device_by_identifier("APSK21")
      assert found != nil
      assert found.identifier == "APS???"
      assert found.model != nil
      assert found.vendor != nil
    end
  end
end
