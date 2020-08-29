#!/bin/sh
(cd $(dirname $0)/.. && guile --r7rs -L . -L ./srfi ./srfi/run-r7rs-checks.guile.scm ./180/checks.sld $1)
