# hs-bt-tracker: Haskell UDP BitTorrent Tracker

Proof-of-concept UDP BitTorrent tracker written in Haskell. I get about 10k
responses per second on my laptop (Intel i7-3820QM, 16GB RAM, macOS Yosemite).

Aims towards implementing the protocol available at
http://www.libtorrent.org/udp_tracker_protocol.html.

Doesn't support black- or whitelisting.

This is more or less a refactored version of a tracker prototype I developed
and later rewrote in Rust (https://github.com/greatest-ape/rs_bt_tracker).
That one has more features, a lot better performance and better tests.

I might still make improvements to `hs-bt-tracker`, but the Rust project is
my primary tracker project.

## Setup

Install `stack` if you haven't already.

To build and start the server, run:

```sh
stack setup # Only necessary once
./scripts/build-and-run.sh
```

## Future plans

See `TODO.md`.
