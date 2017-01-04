# hs-bt-tracker: Haskell UDP BitTorrent Tracker

Proof-of-concept UDP BitTorrent tracker written in Haskell. I get about 130k
responses per second on commodity hardware (i5-2400 running Ubuntu 16.04).

Aims towards implementing the protocol available at
http://www.libtorrent.org/udp_tracker_protocol.html.

Doesn't support black- or whitelisting.

This is more or less a refactored version of a tracker prototype I developed
in Haskell and later [rewrote in Rust](https://github.com/greatest-ape/rs_bt_tracker).
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
