#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

cabal new-run demo-playground -- \
    --system-start "$now" --slot-duration 2 $1\
    submit \
    -t demo-playground/simple-topology.json -n $2 \
    --address $3 --amount $4 \
    -l demo-playground/log-config-submission.yaml
