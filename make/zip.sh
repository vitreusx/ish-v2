#!/usr/bin/env bash

dir=jakub_bednarz
rm -rf $dir jakub_bednarz.zip
rsync -a Makefile good bad make stack.yaml package.yaml src README LICENSE $dir
zip -r jakub_bednarz.zip $dir
rm -rf $dir
