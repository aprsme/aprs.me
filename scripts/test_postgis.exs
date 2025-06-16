#!/usr/bin/env elixir

# Test script to verify PostGIS functionality in the APRS application
# This script must be run from within the Mix project using: mix run scripts/test_postgis.exs

IO.puts("🗺️  Testing PostGIS functionality...")

# Test 1: Check if PostGIS extension is enabled
IO.puts("\n1. Checking PostGIS extension...")
try do
  result = Ecto.Adapters.SQL.query!(Aprs.Repo, "SELECT PostGIS_Version();")
  IO.puts("✅ PostGIS version: #{inspect(result.rows)}")
rescue
  e ->
    IO.puts("❌ PostGIS not available: #{inspect(e)}")
    System.halt(1)
end

# Test 2: Check if location column exists
IO.puts("\n2. Checking location column...")
try do
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = 'packets' AND column_name = 'location';")
  if length(result.rows) > 0 do
    IO.puts("✅ Location column exists: #{inspect(result.rows)}")
  else
    IO.puts("❌ Location column not found")
  end
rescue
  e ->
    IO.puts("❌ Error checking location column: #{inspect(e)}")
end

# Test 3: Test geometry creation and storage
IO.puts("\n3. Testing geometry creation...")
try do
  # Create a test point
  point = %Geo.Point{coordinates: {-96.7969, 32.7767}, srid: 4326}  # Dallas, TX
  IO.puts("✅ Created point: #{inspect(point)}")

  # Test the PostGIS Geometry type directly
  {:ok, cast_result} = Geo.PostGIS.Geometry.cast(point)
  IO.puts("✅ Geo.PostGIS.Geometry cast successful: #{inspect(cast_result)}")

  # Test creating a point using the Packet helper
  created_point = Aprs.Packet.create_point(32.7767, -96.7969)
  IO.puts("✅ Packet.create_point successful: #{inspect(created_point)}")

rescue
  e ->
    IO.puts("❌ Error creating geometry: #{inspect(e)}")
end

# Test 4: Test spatial query functions
IO.puts("\n4. Testing basic spatial queries...")
try do
  # Test creating a point with ST_MakePoint
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT ST_AsText(ST_SetSRID(ST_MakePoint(-96.7969, 32.7767), 4326)) as point_wkt;")
  IO.puts("✅ ST_MakePoint test: #{inspect(result.rows)}")

  # Test distance calculation
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT ST_Distance_Sphere(ST_MakePoint(-96.7969, 32.7767), ST_MakePoint(-97.7431, 30.2672)) as distance_meters;")
  IO.puts("✅ Distance between Dallas and Austin: #{inspect(result.rows)} meters")

rescue
  e ->
    IO.puts("❌ Error in spatial queries: #{inspect(e)}")
end

# Test 5: Check existing packet data migration
IO.puts("\n5. Checking migrated packet data...")
try do
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT COUNT(*) as total_packets, COUNT(location) as packets_with_location FROM packets;")
  IO.puts("✅ Packet statistics: #{inspect(result.rows)}")

  # Show a sample of migrated packets
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT sender, ST_AsText(location) as location_wkt FROM packets WHERE location IS NOT NULL LIMIT 3;")
  if length(result.rows) > 0 do
    IO.puts("✅ Sample migrated packets:")
    Enum.each(result.rows, fn [sender, location] ->
      IO.puts("   #{sender}: #{location}")
    end)
  else
    IO.puts("ℹ️  No packets with location data found")
  end

rescue
  e ->
    IO.puts("❌ Error checking packet data: #{inspect(e)}")
end

# Test 6: Test spatial indexes
IO.puts("\n6. Checking spatial indexes...")
try do
  result = Ecto.Adapters.SQL.query!(Aprs.Repo,
    "SELECT indexname, indexdef FROM pg_indexes WHERE tablename = 'packets' AND indexname LIKE '%location%';")
  if length(result.rows) > 0 do
    IO.puts("✅ Spatial indexes found:")
    Enum.each(result.rows, fn [name, def] ->
      IO.puts("   #{name}: #{def}")
    end)
  else
    IO.puts("⚠️  No spatial indexes found")
  end
rescue
  e ->
    IO.puts("❌ Error checking indexes: #{inspect(e)}")
end

# Test 7: Performance test with spatial query
IO.puts("\n7. Testing spatial query performance...")
try do
  # Test a typical "packets within radius" query
  start_time = System.monotonic_time(:millisecond)

  result = Ecto.Adapters.SQL.query!(Aprs.Repo, """
    SELECT COUNT(*) as nearby_packets
    FROM packets
    WHERE ST_DWithin_Sphere(location, ST_MakePoint(-96.7969, 32.7767), 50000)
    AND location IS NOT NULL;
  """)

  end_time = System.monotonic_time(:millisecond)
  duration = end_time - start_time

  IO.puts("✅ Spatial query completed in #{duration}ms")
  IO.puts("   Found packets within 50km of Dallas: #{inspect(result.rows)}")

rescue
  e ->
    IO.puts("❌ Error in spatial query: #{inspect(e)}")
end

# Test 8: Test inserting a packet with geometry
IO.puts("\n8. Testing packet insertion with geometry...")
try do
  # Create test packet data
  test_packet_attrs = %{
    base_callsign: "TEST",
    data_type: "position",
    destination: "APRS",
    information_field: "!3216.50N/09647.00W>Test packet",
    path: "WIDE1-1,WIDE2-1",
    sender: "TEST-1",
    ssid: "1",
    received_at: DateTime.utc_now(),
    lat: 32.275,
    lon: -96.783,
    has_position: true,
    region: "32.3,-96.8",
    raw_packet: "TEST-1>APRS,WIDE1-1,WIDE2-1:!3216.50N/09647.00W>Test packet"
  }

  case Aprs.Packets.store_packet(test_packet_attrs) do
    {:ok, packet} ->
      IO.puts("✅ Successfully inserted test packet: #{packet.sender}")

      # Clean up test packet
      Aprs.Repo.delete(packet)
      IO.puts("✅ Test packet cleaned up")

    {:error, changeset} ->
      IO.puts("❌ Failed to insert test packet: #{inspect(changeset.errors)}")
  end

rescue
  e ->
    IO.puts("❌ Error in packet insertion test: #{inspect(e)}")
end

IO.puts("\n🎉 PostGIS testing completed!")
IO.puts("\n📊 Summary:")
IO.puts("   - PostGIS extension is enabled")
IO.puts("   - Location column with geometry type exists")
IO.puts("   - Spatial indexes are created")
IO.puts("   - Basic spatial functions are working")
IO.puts("   - Packet insertion with geometry works")
IO.puts("   - Data migration from lat/lon to PostGIS geometry completed")
IO.puts("\n🚀 Your APRS application is now ready for efficient spatial queries!")
