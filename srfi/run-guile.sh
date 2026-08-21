#!/bin/sh

# SPDX-FileCopyrightText: 2026 Martin Becze
#
# SPDX-License-Identifier: MIT

(cd $(dirname $0)/.. && guile --r7rs -L . -L ./srfi ./srfi/run-r7rs-checks.guile.scm ./180/checks.sld $1)
