#!/usr/bin/env bash
#

set -o pipefail
set -e
GREP="egrep --line-buffered"

run211 () {
  # Look ma I'm testing pipefail.
  if false | cat; then
    echo "Failing pipe didn't fail!" && exit 1
  fi

  if sbt console <.ci/repl.txt |& grep -q 'Compilation Failed'; then
    echo "console smoke test encountered error" && exit 1
  fi

  sbt ++$TRAVIS_SCALA_VERSION -batch -no-colors cover
}
run212 () {
  # High fucking tech right here.
  # https://github.com/scoverage/sbt-scoverage/issues/96
  SED="sed"
  which gsed >/dev/null && SED="gsed"
  $SED --in-place 's/.*coverage.*//' ./project/plugins.sbt

  sbt ++$TRAVIS_SCALA_VERSION -batch -no-colors test
  git checkout HEAD -- ./project/plugins.sbt
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
