#!/bin/bash
stack build && stack exec -- hs-bt-tracker-bin +RTS -N
