import static io.undertow.Undertow.*;
import static io.undertow.UndertowOptions.*;
import io.undertow.server.*;
import io.undertow.util.*;
import java.net.InetAddress;

public class Main {

  public static void main(String[] args) throws Exception {
    final String hostname = "::";
    final int port = 3000;
    System.out.println("Running the server on host=> " + hostname + " port=> " + port);
    builder()
      .addHttpListener(port, hostname)
      .setServerOption(ENABLE_HTTP2, true)
      .setHandler(new HttpHandler() {
        @Override
        public void handleRequest(final HttpServerExchange exchange) throws Exception {
          exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
          exchange.getResponseSender().send("Hello, World!");
        }
      })
      .build()
      .start();
  }

}
