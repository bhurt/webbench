import java.util.*;
import java.sql.*;
import javax.sql.*;
import org.postgresql.ds.PGSimpleDataSource;
import static java.lang.System.getProperty;

public class WebbenchBaseDataSource extends PGSimpleDataSource {

  public WebbenchBaseDataSource() {
    this.setServerName(getProperty("PGHOST", "localhost"));
    this.setDatabaseName(getProperty("PGDATABASE", "webbench"));
    this.setPortNumber(Integer.parseInt(getProperty("PGPORT", "5432")));
    this.setUser(getProperty("PGUSER", "webbench"));
    this.setPassword(getProperty("PGPASSWORD", ""));
  }

}
