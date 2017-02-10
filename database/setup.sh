#!/bin/bash -exu

HEREDIR=`dirname $0`
cd $HEREDIR
HEREDIR=`pwd`

cd ~postgres

sudo -u postgres dropdb --if-exists -e -w webbench
sudo -u postgres dropuser --if-exists -e -w webbench
sudo -u postgres createuser -e -E -w webbench
sudo -u postgres createdb -e --owner=webbench -w webbench

CONFIG_FILE='/etc/postgresql/9.6/main/pg_hba.conf'
if sudo grep webbench $CONFIG_FILE
then
  echo "Already have webbench defined in the user config file"
else
  sudo cp /etc/postgresql/9.6/main/pg_hba.conf /etc/postgresql/9.6/main/pg_hba.conf.old
  echo -e "\nlocal sameuser webbench trust\n" | sudo tee /etc/postgresql/9.6/main/pg_hba.conf
  sudo cat /etc/postgresql/9.6/main/pg_hba.conf | sudo tee -a /etc/postgresql/9.6/main/pg_hba.conf
  sudo /etc/init.d/postgresql restart
fi

cat "$HEREDIR/dbinit.sql" | psql webbench webbench
