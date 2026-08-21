#!/bin/sh

# SPDX-FileCopyrightText: 2026 Amirouche Boubekki
#
# SPDX-License-Identifier: MIT

chibi-scheme -I $(pwd)/.. run-r7rs-checks.scm 180/checks.sld $1
