#!/usr/bin/env bash
#

set -o pipefail
set -e
GREP="egrep --line-buffered"

# Look ma I'm testing pipefail.
if /usr/bin/false | cat; then echo "Failing pipe didn't fail!" && exit 1; fi

runTests () {
  sbt -batch console # just making sure it compiles
  sbt -batch -no-colors cover
}

runTests \
  |& $GREP -v '^\[info\][ ]?(Resolving|Waiting|Generating|\[info\]|[#]|$)' \
  |  $GREP -v '^(Download|Resolving|Fetching|Resolution[ ]done|Bye[!])' \
  |  $GREP -v '(nstrumentation|Cobertura|HTML)'
