#!/usr/bin/env bash
 
find bad/*.ish good/*.ish \
    -exec printf ">> {}\n" \;\
    -exec ./interpreter {} 2>&1 \;
