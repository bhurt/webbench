Web Benchmarks
=======================

Running the Benchmarks
===============================

All of these benchmarks run the same way -- you provide environment variables to configure the database connection, and then run the
servers, and they provide the endpoints necessary for the benchmarking on port 3000, listening on all interfaces.

Environment Variables
-------------------------

* `PGHOST` -- The hostname where the database lives.
* `PGPORT` -- The port where the database is listening.
* `PGDATABASE` -- The database name to connect to.
* `PGUSER` -- The username to use to connect to the database.
* `PGPASSWORD` -- The password to use for the username.

Running the Servers
----------------------

Each implementation contains a `run.sh` script which runs the server.
