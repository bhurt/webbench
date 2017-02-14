import static io.undertow.Undertow.*;
import static io.undertow.UndertowOptions.*;
import io.undertow.server.*;
import io.undertow.server.handlers.*;
import io.undertow.util.*;
import java.net.InetAddress;
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

  private static <T extends DataSource> T createDataSource() {
    return new WebbenchDataSource();
  }

  private static <T extends HttpHandler> T createHandler(final DataSource ds) {
    return new CanonicalPathHandler(createPathHandler(ds));
  }

  private static <T extends HttpHandler> T createPathHandler(final DataSource ds) {
    final PathHandler handler = new PathHandler(createUsersByIdHandler(ds));
    handler.addExactPath("/users", createUsersHandler(ds));
    return handler;
  }

  private static <T extends HttpHandler> T createUsersByIdHandler(final DataSource ds) {
    final PathTemplateHandler handler = new PathTemplateHandler();
    handler.add("/users/{userId}", createUserIdHandler(ds));
    return handler;
  }

  private static UsersHandler createUsersHandler(final DataSource ds) {
    return new UsersHandler(ds);
  }

  private static UserIdHandler createUserIdHandler(final DataSource ds) {
    return new UserIdHandler(ds);
  }

}
