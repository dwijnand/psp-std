#!/usr/bin/env bash
#

set -euo pipefail

: ${TRAVIS_SCALA_VERSION:=2.12.1}

GREP="egrep --line-buffered"
SBT="sbt ++$TRAVIS_SCALA_VERSION -no-colors"

# restore stty settings (echo in particular)
onRunnerExit() {
  [[ -f project/scoverage.sbt ]] || git checkout HEAD -- project/scoverage.sbt
}

runTests () {
  # Look ma I'm testing pipefail.
  if false | cat; then
    echo "Failing pipe didn't fail!" && exit 1
  fi

  if $SBT console <.ci/repl.txt |& grep -q 'Compilation Failed'; then
    echo "console smoke test encountered error" && exit 1
  fi

  $SBT -batch cover
  $SBT -batch macros/test
  codecov
}

runTests \
  |& $GREP -v '^\[info\][ ]?(Resolving|Waiting|Generating|\[info\]|[#]|$)' \
  |  $GREP -v '^(Download|Resolving|Fetching|Resolution[ ]done|Bye[!])' \
  |  $GREP -v '(nstrumentation|Cobertura|HTML)'
