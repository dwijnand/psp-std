#!/usr/bin/env bash
#

set -e
: ${JMH_ARGS:= -f1 -t1 }

runTests () {
  sbt -batch -no-colors "benchmark/jmh:run $JMH_ARGS"
  sbt -batch -no-colors cover
}

runTests \
  | egrep -v '^\[info\] (Resolving|Waiting|\# Warmup|Generating|\[info\])' \
  | egrep -v '^(Download|Resolving|Fetching)' \
  | egrep -v '(nstrumentation|Cobertura|HTML)'
