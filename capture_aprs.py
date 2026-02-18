#!/usr/bin/env python3
"""
Connect to dallas.aprs2.net APRS-IS server and write all packets to packets.txt.
Uses receive-only login (passcode -1) so no amateur license required.
"""

import socket
import sys
import signal
from datetime import datetime

HOST = "dallas.aprs2.net"
PORT = 10152  # Full feed, no filter required (use 14580 with a filter)
OUTPUT_FILE = "packets.txt"

running = True


def signal_handler(sig, frame):
    global running
    print("\nShutting down...")
    running = False


signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)


def main():
    print(f"Connecting to {HOST}:{PORT}...")

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.connect((HOST, PORT))
        sock.settimeout(30)

        f = sock.makefile("r", encoding="utf-8", errors="replace")

        # Read server banner
        banner = f.readline().strip()
        print(f"Server: {banner}")

        # Login - passcode -1 for receive-only
        login = "user N0CALL pass -1 vers APRSCapture 1.0\r\n"
        sock.sendall(login.encode())

        # Read login response
        response = f.readline().strip()
        print(f"Login: {response}")

        print(f"Writing packets to {OUTPUT_FILE} (Ctrl+C to stop)...")

        count = 0
        with open(OUTPUT_FILE, "w", encoding="utf-8") as out:
            out.write(f"# APRS capture from {HOST}:{PORT}\n")
            out.write(f"# Started: {datetime.utcnow().isoformat()}Z\n\n")

            while running:
                try:
                    line = f.readline()
                    if not line:
                        print("Connection closed by server.")
                        break

                    line = line.strip()
                    if not line:
                        continue

                    # Skip comments (server messages start with #)
                    out.write(line + "\n")
                    out.flush()
                    count += 1

                    if count % 100 == 0:
                        print(f"  {count} packets captured...")

                except socket.timeout:
                    continue
                except Exception as e:
                    print(f"Error: {e}")
                    break

        print(f"Done. Captured {count} packets to {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
