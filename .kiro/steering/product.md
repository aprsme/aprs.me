# Product Overview

Aprsme is an APRS (Automatic Packet Reporting System) web application that provides real-time tracking and visualization of amateur radio packets. The application connects to APRS-IS servers to receive and process amateur radio position reports, weather data, and other telemetry.

## Core Features

- **Real-time Map Visualization**: Interactive map showing APRS stations and their positions
- **Packet Processing**: Ingests and processes APRS packets from APRS-IS network
- **Station Information**: Detailed views of individual callsigns and their activity
- **Weather Data**: Weather station reporting and visualization
- **Bad Packet Analysis**: Monitoring and analysis of malformed packets
- **Multi-language Support**: Internationalization with English, Spanish, German, and French
- **User Authentication**: User accounts and session management
- **API Access**: RESTful API for programmatic access to data

## Technical Architecture

The application uses a GenStage-based packet processing pipeline to handle high-volume APRS data streams, with PostgreSQL for persistence and Phoenix LiveView for real-time web interfaces. Background job processing is handled by Oban for maintenance tasks like packet cleanup.
