#!/bin/sh
# Tirado de https://stackoverflow.com/questions/49395778/how-do-i-locally-source-environment-variables-that-i-have-defined-in-a-docker-fo
source <(sed -E -e "s/^([^#])/export \1/" -e "s/=/='/" -e "s/(=.*)$/\1'/" $1)