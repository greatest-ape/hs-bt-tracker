# TODO

## Important

  - Read config from a file
  - Add config file location as a command line argument
  - Add setting for maximum accepted torrents in scrape request

## Less important

  - Use better socket model to enable multithreading. Maybe have one thread
    do all socket access and use chans to communicate with other threads?
  - Do memory usage profiling and try to reduce memory footprint
  - Add QuickCheck tests
  - Does the scrape response return peer stats in the same order as the info
    hashes were received?
  - Is the name of the "State" type misleading?
  - Handle word/int size protocol discrepancies (does anything really need to
    be done?)

## Won't fix

  - Use utils for all AppM reader access operations (see announce handler)
