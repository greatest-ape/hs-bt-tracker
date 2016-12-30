#!/bin/bash
stack build && stack exec -- hs-bt-tracker +RTS -N
