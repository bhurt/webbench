package webbench;

import org.postgresql.ds.PGSimpleDataSource;

public class WebbenchBaseDataSource extends PGSimpleDataSource {

  public WebbenchBaseDataSource() {
    this.setServerName(EnvUtils.getEnv("PGHOST", "localhost"));
    this.setDatabaseName(EnvUtils.getEnv("PGDATABASE", "webbench"));
    this.setPortNumber(EnvUtils.getEnv("PGPORT", 5432, Integer::parseInt));
    this.setUser(EnvUtils.getEnv("PGUSER", "webbench"));
    this.setPassword(EnvUtils.getEnv("PGPASSWORD", ""));
  }

}
