#!/bin/bash -exu

HEREDIR=`dirname $0`
cd $HEREDIR
HEREDIR=`pwd`

: ${PGHOST:=localhost}
export PGHOST
: ${PGPORT:=5432}
export PGPORT
: ${PSQL_USER:=webbench}
: ${PSQL_PASSWORD:=w3bb3nch}
: ${PSQL_DATABASE:=webbench}

cd ~postgres

sudo -u postgres dropdb --if-exists -e -w "$PSQL_DATABASE"
sudo -u postgres dropuser --if-exists -e -w "$PSQL_USER"
sudo -u postgres createuser -e -E -w "$PSQL_USER"
echo "ALTER USER $PSQL_USER WITH PASSWORD '$PSQL_PASSWORD'" | sudo -u postgres psql -a -w
sudo -u postgres createdb -e --owner=$PSQL_USER -w "$PSQL_DATABASE"

# Now store the password in a .pgpass file
export PGPASSFILE="$HEREDIR/.pgpass"
echo "Creating the postgresql password file at $PGPASSFILE"
export PGUSER=$PSQL_USER
export PGDATABASE=$PSQL_DATABASE
touch "$PGPASSFILE"
chmod -vv 600 "$PGPASSFILE"
echo -e "$PGHOST:$PGPORT:$PGDATABASE:$PSQL_USER:$PSQL_PASSWORD\n" | tee "$PGPASSFILE"
chmod -vv 400 "$PGPASSFILE"

cat "$HEREDIR/dbinit.sql" | pv | psql

cd "$HEREDIR"
bundle install --path=gems && bundle exec ruby ./populate.rb

rm -f "$PGPASSFILE"
