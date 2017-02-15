const Promise = require("bluebird");

const pgp = require("pg-promise")({
  promiseLib: Promise
});

const db = pgp({
  host: process.env.PGHOST,
  port: process.env.PGPORT,
  database: process.env.PGDATABASE,
  user: process.env.PGUSER,
  password: process.env.PGPASSWORD,
  poolSize: 1000,
  returnToHead: true
});

module.exports = db;
