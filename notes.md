# APRS.me Project Exploration Notes

## Overview
- This is a Phoenix LiveView application for APRS (Automatic Packet Reporting System)
- APRS is a digital communications system used by amateur radio operators
- The app appears to track and display APRS packets/messages

## Project Structure
- Standard Phoenix 1.7+ structure with LiveView
- Uses Elixir ~> 1.17
- Has authentication system (user auth files present)
- Contains map functionality (map_live modules)
- Has packet tracking (packets_live, status_live)
- Database with PostGIS for geographic data
- Real-time features with PubSub and presence

## Key Components Found
- Map visualization (Leaflet integration)
- Packet display and filtering
- User authentication
- Real-time updates
- Weather data support
- Geographic/spatial queries

## Next Steps
- Check if deps are installed
- Explore the database setup
- Start the server to see the current state
