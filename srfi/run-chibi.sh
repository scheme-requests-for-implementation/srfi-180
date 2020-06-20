#!/bin/sh
chibi-scheme -I $(pwd)/.. run-r7rs-checks.scm 180/checks.sld $1
