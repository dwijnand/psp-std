#!/usr/bin/env bash
#

set pipefail
set -e
: ${JMH_ARGS:= -f1 -t1 }

runTests () {
  sbt -batch console # just making sure it compiles
  sbt -batch -no-colors "benchmark/jmh:run $JMH_ARGS"
  sbt -batch -no-colors cover
}

runTests \
  |& egrep -v --line-buffered '^\[info\] (Resolving|Waiting|\# Warmup|Generating|\[info\])' \
  |  egrep -v --line-buffered '^(Download|Resolving|Fetching|Resolution[ ]done|Bye[!])' \
  |  egrep -v --line-buffered '(nstrumentation|Cobertura|HTML)'
