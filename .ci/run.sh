#!/usr/bin/env bash
#

set pipefail
set -e
GREP="egrep --line-buffered"

runTests () {
  sbt -batch console # just making sure it compiles
  sbt -batch -no-colors cover
}

runTests \
  |& $GREP -v '^\[info\][ ]?(Resolving|Waiting|Generating|\[info\]|[#]|$)' \
  |  $GREP -v '^(Download|Resolving|Fetching|Resolution[ ]done|Bye[!])' \
  |  $GREP -v '(nstrumentation|Cobertura|HTML)'
