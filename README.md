# hs-bt-tracker: Haskell UDP BitTorrent Tracker

Proof-of-concept UDP BitTorrent tracker written in Haskell. I get about 10k
responses per second on my laptop (Intel i7-3820QM, 16GB RAM, macOS Yosemite).

Aims towards implementing the protocol available at
http://www.libtorrent.org/udp_tracker_protocol.html.

Doesn't support black- or whitelisting.

Doesn't have any tests built in and doesn't necessarily even work well!

## Setup

Install `stack` if you haven't already.

To build and start the server, run:

```sh
./scripts/build-and-run.sh
```

## Future plans

Better performance, better test coverage and other improvements. See `TODO.md`.
