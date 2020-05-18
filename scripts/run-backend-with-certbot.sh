#!/usr/bin/env bash
set -e
trap "set +e" 0

if [ ! -f "$CERTKEYPATH" ] && [ ! -f "$CERTPATH" ]; then
    $2 &
    # Crappy way to do it, but give some time for the app to start listening and be able to serve ACME's challenge
    sleep 3
    mkdir -p $BACKEND_STATIC_FILES_PATH
    $1
    echo "certbot has run, killing and restarting the backend now"
    pkill -x "$2"
    echo "Killed backend process. Waiting a bit to make sure listen port will be available"
    sleep 5
fi

exec $2

set +e