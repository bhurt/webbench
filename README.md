Web Server Benchmarks of Real Business Value
===============================================

The goal of these benchmarks is to get away from the hyper-tuned microbenchmarks that so fascinate certain geeks, and instead measure
a real-world usage scenario. Specifically, the question we are pursuing is:

> How financially expensive is it to run a REST API web server which wraps a database with production levels of data and load?

The way that we break this down is as follows:

* The key financial metric that we are tracking is dollars per concurrent user. We are going to use the MythBusters approach to
  gathering these metrics: first, we are going to test the load level under an extreme case; then, we will see what it would take
  to accomplish the load level we have set out.
  * The extreme case that we will test is using a single `t2.nano` as the server:
  this is 0.5GiB of RAM, a variable amount of compute capability, and (as of this writing) USD$0.0059 per hour. We will test
  each implementation utilizing just one of these servers under various levels of load.
  * The other extreme test will be using the compute-optimized `c4.xlarge` as the underlying image:
  4 CPUs, 16 [ECU](https://aws.amazon.com/ec2/faqs/#What_is_an_EC2_Compute_Unit_and_why_did_you_introduce_it), and 7.5GiB of RAM.
  They run USD$0.10 per hour (that's 17 `t2.nano` instances per hour). We will test each implementation utilizing just one of
  these servers under the highest level of load, so that implementations which could barely run under `t2.nano` have their chance
  to shine, too.
  * We will then set up an Auto-Scaling Group of the `t2.nano` servers, and see how many servers the Auto-Scaling Group spins
  up while under this load, along with the performance characteristics of that Auto-Scaling Group from the client's perspective,
  along with the cost of those Auto-Scaling Groups from a business perspective.  (If you would like to see this test repeated
  with the `c4.xlarge` instances, please contact the developers with your sponshorship offer.)

Some notes:

* What we are curious in measuring is the REST API web server, not the database. Although we want the database to have performance
  characteristics in line with production usage, we do not need to be exercising different kinds of operations against the database,
  so testing only database reads is fine. Along these same lines, we need to ensure that the database is identical across all the
  different tests, and that it has a chance to warm its caches before we take measurements.
* We are also curious in measuring code such as what you would find in the real world. All the code should resemble something that
  would end up in production. Similarly, we are not going to do significant tuning to the infrastructure underlying the codebase,
  but instead run on the default settings for the various platforms/frameworks.
* Servers that die through the course of the benchmark will be automatically restarted.

Server Configuration
=====================

All of these benchmark servers run the same way -- you provide environment variables to configure the database
connection, and then run the servers, and they provide the endpoints necessary for the benchmarking on port `3000`,
listening on all interfaces.

Environment Variables
-------------------------

* `PGHOST` -- The hostname where the database lives.
* `PGPORT` -- The port where the database is listening.
* `PGDATABASE` -- The database name to connect to.
* `PGUSER` -- The username to use to connect to the database.
* `PGPASSWORD` -- The password to use for the username.

Running the Servers
----------------------

Each implementation must contain a `run.sh` script which runs the server.
