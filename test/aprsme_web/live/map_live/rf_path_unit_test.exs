defmodule AprsmeWeb.MapLive.RfPathUnitTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.MapLive.RfPath

  describe "parse_rf_path/1" do
    test "extracts digipeater and igate from qAR path" do
      # qAR = packet received by igate from RF
      # Format: [digipeaters...],qAR,[igate]
      result = RfPath.parse_rf_path("1BATES-5*,WIDE2-1,qAR,LU9EHU-10")
      assert "1BATES-5" in result
      assert "LU9EHU-10" in result
    end

    test "extracts igate from simple qAR path with no digipeaters" do
      result = RfPath.parse_rf_path("2E0FKC-10*,qAR,M0SDM-10")
      assert "2E0FKC-10" in result
      assert "M0SDM-10" in result
    end

    test "extracts digipeaters and igate from qAS path" do
      # qAS = packet sent to APRS-IS server
      result = RfPath.parse_rf_path("1BATES-5,LU7AMC-15*,WIDE1,qAS,LU4DQ-1")
      assert "1BATES-5" in result
      assert "LU7AMC-15" in result
      assert "LU4DQ-1" in result
    end

    test "extracts digipeaters and igate from qAO path" do
      # qAO = gated by positioned igate
      result = RfPath.parse_rf_path("1BATES-5*,LU9EBZ,qAO,LU4AGC-10")
      assert "1BATES-5" in result
      assert "LU9EBZ" in result
      assert "LU4AGC-10" in result
    end

    test "filters out WIDE aliases" do
      result = RfPath.parse_rf_path("K5GVL-10*,WIDE1-1,WIDE2-1,qAR,N5TXZ-10")
      refute Enum.any?(result, &String.starts_with?(&1, "WIDE"))
      assert "K5GVL-10" in result
      assert "N5TXZ-10" in result
    end

    test "filters out RELAY, TRACE, and other aliases" do
      result = RfPath.parse_rf_path("KC5ABC-9,RELAY,TRACE3-3,ECHO,HOP7-7,qAR,K5VOM-10")
      refute "RELAY" in result
      refute "ECHO" in result
      assert "KC5ABC-9" in result
      assert "K5VOM-10" in result
    end

    test "strips asterisks from used digipeater flags" do
      result = RfPath.parse_rf_path("WA5VHU-8*,WIDE1*,qAR,K5VOM-10")
      assert "WA5VHU-8" in result
      refute "WA5VHU-8*" in result
    end

    test "returns empty for TCPIP paths" do
      assert RfPath.parse_rf_path("TCPIP*,qAC,T2TEXAS") == []
    end

    test "returns empty for NOGATE paths" do
      assert RfPath.parse_rf_path("NOGATE,qAC,SERVER") == []
    end

    test "returns empty for empty string" do
      assert RfPath.parse_rf_path("") == []
    end

    test "returns empty for nil" do
      assert RfPath.parse_rf_path(nil) == []
    end

    test "handles path with numeric-only prefix elements" do
      # Paths like "-1,-2,qAR,DB0FTS-10" have non-callsign elements
      result = RfPath.parse_rf_path("-1,-2,qAR,DB0FTS-10")
      assert "DB0FTS-10" in result
      refute "-1" in result
      refute "-2" in result
    end

    test "handles path with only igate (no RF digipeaters)" do
      result = RfPath.parse_rf_path("-1,qAO,BH4CRV-15")
      assert "BH4CRV-15" in result
      refute "-1" in result
    end

    test "returns unique callsigns" do
      result = RfPath.parse_rf_path("K5ABC-10*,K5ABC-10,qAR,K5ABC-10")
      assert length(Enum.uniq(result)) == length(result)
    end

    test "handles complex multi-hop path" do
      result = RfPath.parse_rf_path("3A2ARM-10,WIDE1*,qAR,3A2MT-10")
      assert "3A2ARM-10" in result
      assert "3A2MT-10" in result
      refute Enum.any?(result, &String.starts_with?(&1, "WIDE"))
    end

    test "handles DMR paths" do
      result = RfPath.parse_rf_path("5B4AIE,DMR*,qAR,5B4AIE")
      assert "5B4AIE" in result
      refute "DMR" in result
    end
  end
end
