#!/usr/bin/env bash
#

# set -e

sbt -J-Xmx3784m -batch -no-colors ++2.11.8 cover |& egrep -v '^\[info\] Resolving '

# sbt updateImpactSubmit || true
