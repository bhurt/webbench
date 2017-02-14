import java.util.*;
import java.sql.*;
import javax.sql.*;
import org.postgresql.ds.PGSimpleDataSource;
import static java.lang.System.getProperty;
import static com.google.common.base.Preconditions.*;

public class WebbenchDataSource extends HikariDataSource {

  public WebbenchDataSource() {
    this.setDataSourceClassName(WebbenchBaseDataSource.class.getName());
    this.setConnectionTimeout(120000);
    this.setMaximumPoolSize(getMaximumPoolSize());
    this.setMaxLifetime(300000);
    this.setIdleTimeout(60000);
  }

  private static int getMaximumPoolSize() {
    try(final Connection conn = new WebbenchBaseDataSource().getConnection()) {
      try(final Statement stmt = conn.createStatement()) {
        try(final ResultSet rs = stmt.executeQuery("SHOW max_connections")) {
          checkState(rs.next(), "No results from max_connections");
          return (int)Math.floor(rs.getInt(1) * 0.9);
        }
      }
    }
  }

}
