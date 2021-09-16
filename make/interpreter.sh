#!/usr/bin/env bash

./stack build
rsync "$(./stack exec -- whereis ish-v2 | awk '{print $2}')" interpreter
