#!/bin/bash -exu

HEREDIR=`dirname $0`
cd "$HEREDIR"

../gradlew clean build
exec java -jar ./build/libs/webbench-spring-boot.jar
