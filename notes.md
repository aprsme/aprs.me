# APRS.me Project Exploration Notes

## Overview
- This is a Phoenix LiveView application for APRS (Automatic Packet Reporting System)
- APRS is a digital communications system used by amateur radio operators
- The app tracks and displays APRS packets/messages in real-time on an interactive map

## Project Structure
- Standard Phoenix 1.7+ structure with LiveView
- Uses Elixir ~> 1.17
- Has authentication system (user auth files present)
- Contains map functionality (map_live modules)
- Has packet tracking (packets_live, status_live)
- Database with PostGIS for geographic data
- Real-time features with PubSub and presence

## Key Features Found
- **Interactive Map**: Leaflet-based map with OpenStreetMap tiles
- **Real-time APRS Tracking**: Connected to APRS-IS (dallas.aprs2.net:14580)
- **Responsive Design**: Slideover panel for controls, mobile-friendly
- **Search Functionality**: Callsign search capability
- **Trail Display**: Configurable trail duration (1 hour to 1 week)
- **Historical Data**: Historical packet loading (1-24 hours)
- **Packet Display**: Multiple views (map, packets, status, weather, bad packets)
- **User Authentication**: Complete auth system ready
- **API Endpoints**: RESTful API for callsign data

## Technical Stack
- **Backend**: Phoenix 1.7, Elixir 1.17, Ecto with PostgreSQL + PostGIS
- **Frontend**: LiveView, Tailwind CSS, Leaflet.js for maps
- **Real-time**: Phoenix PubSub, WebSocket connections
- **Background Jobs**: Oban for job processing
- **External Integration**: APRS-IS network connection
- **Deployment**: Docker-ready with releases

## Current Status
- ✅ Application runs successfully on localhost:4000
- ✅ Database migrations completed
- ✅ APRS-IS connection established
- ✅ Map interface working with controls
- ✅ Real-time packet processing active
- ⚠️  Some asset loading optimizations possible
- 📡 Currently showing "no internet" (likely due to APRS data feed)

## Next Steps
- Explore specific LiveView modules
- Check packet data flow
- Review real-time update mechanisms
- Examine API capabilities

