#!/usr/bin/env bash

wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz
tar xvf linux-x86_64.tar.gz
rsync stack-2.7.1-linux-x86_64/stack .
