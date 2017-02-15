import com.zaxxer.hikari.HikariDataSource;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.concurrent.TimeUnit;

import static com.google.common.base.Preconditions.checkState;

public class WebbenchDataSource extends HikariDataSource {

  public WebbenchDataSource() {
    this.setDataSourceClassName(WebbenchBaseDataSource.class.getName());
    this.setConnectionTimeout(TimeUnit.HOURS.toMillis(1L));
    this.setMaximumPoolSize(fetchMaximumPoolSize());
    this.setIdleTimeout(TimeUnit.MINUTES.toMillis(1L));
  }

  private static int fetchMaximumPoolSize() {
    try(final Connection conn = new WebbenchBaseDataSource().getConnection()) {
      try(final Statement stmt = conn.createStatement()) {
        try(final ResultSet rs = stmt.executeQuery("SHOW max_connections")) {
          checkState(rs.next(), "No results from max_connections");
          return (int)Math.max(1, Math.floor(rs.getInt(1) * 0.9));
        }
      }
    } catch(RuntimeException re) {
      throw re;
    } catch(Exception e) {
      throw new RuntimeException(e);
    }
  }

}
