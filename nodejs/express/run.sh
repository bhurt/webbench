#!/bin/bash -exu

HEREDIR=`dirname $0`
cd "$HEREDIR"

npm install && npm dedupe
exec node ./index.js
