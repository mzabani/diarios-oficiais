#!/usr/bin/env bash

sed -E "s/^ *$1 *= *\"?([^\"]*)\"?\$/\\1/;t;d" $2