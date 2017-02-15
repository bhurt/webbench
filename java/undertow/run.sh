#!/bin/bash -exu

HEREDIR=`dirname $0`
cd "$HEREDIR"

../gradlew clean build

cd ./build/distributions/
tar xvf *.tar
exec ./undertow/bin/undertow
