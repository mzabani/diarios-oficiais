#!/bin/sh
set -e

certbot certonly --webroot --agree-tos --config-dir=$CERTBOT_DIR/config-dir/ --work-dir=$CERTBOT_DIR/work-dir --logs-dir=$CERTBOT_DIR/logs-dir -w $BACKEND_STATIC_FILES_PATH -n $CERTBOT_EXTRA_ARGS -d $CERTBOT_HOST

set +e