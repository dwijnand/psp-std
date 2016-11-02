#!/usr/bin/env bash
#

set -euo pipefail

: ${TRAVIS_SCALA_VERSION:=2.11.8}

GREP="egrep --line-buffered"
SBT="sbt ++$TRAVIS_SCALA_VERSION -no-colors"

# restore stty settings (echo in particular)
onRunnerExit() {
  [[ -f project/scoverage.sbt ]] || git checkout HEAD -- project/scoverage.sbt
}

run211 () {
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
run212 () {
  # High fucking tech right here.
  # https://github.com/scoverage/sbt-scoverage/issues/96
  trap onRunnerExit EXIT
  rm ./project/scoverage.sbt
  $SBT -batch test
}
runTests () {
  case $TRAVIS_SCALA_VERSION in
    2.12*) run212 ;;
        *) run211 ;;
  esac
}

runTests \
  |& $GREP -v '^\[info\][ ]?(Resolving|Waiting|Generating|\[info\]|[#]|$)' \
  |  $GREP -v '^(Download|Resolving|Fetching|Resolution[ ]done|Bye[!])' \
  |  $GREP -v '(nstrumentation|Cobertura|HTML)'
