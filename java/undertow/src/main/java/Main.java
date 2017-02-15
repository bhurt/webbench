import static io.undertow.Undertow.*;
import static io.undertow.UndertowOptions.*;
import io.undertow.server.*;
import io.undertow.server.handlers.*;

import javax.sql.DataSource;

public class Main {

  public static void main(String[] args) throws Exception {
    final String hostname = "::";
    final int port = 3000;
    System.out.println("Running the server on host=> " + hostname + " port=> " + port);
    builder()
      .addHttpListener(port, hostname)
      .setServerOption(ENABLE_HTTP2, true)
      .setHandler(createHandler(createDataSource()))
      .build()
      .start();
  }

  private static DataSource createDataSource() {
    return new WebbenchDataSource();
  }

  private static HttpHandler createHandler(final DataSource ds) {
    return new CanonicalPathHandler(createPathHandler(ds));
  }

  private static HttpHandler createPathHandler(final DataSource ds) {
    final PathHandler handler = new PathHandler();
    handler.addExactPath("/users", createUsersHandler(ds));
    return handler;
  }

  private static UsersHandler createUsersHandler(final DataSource ds) {
    return new UsersHandler(ds);
  }

}
