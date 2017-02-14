import io.undertow.server.*;
import io.undertow.server.handlers.*;
import io.undertow.util.*;
import io.undertow.io.*;
import javax.jdbc.*;
import java.jdbc.*;
import static com.google.common.base.Preconditions.*;
import com.google.common.io.*;

public class UsersHandler implements HttpHandler {

  private final DataSource db;

  public UsersHandler(DataSource db) {
    checkNotNull(db, "DataSource may not be null");
    this.db = db;
  }

  public void handleRequest(final HttpServerExchange exchange) {
    checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
    exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
    final String method = getMethod(exchange);
    switch(method) {
      case "GET":
        handleGet(exchange);
        return;
      default:
        handlePost(exchange);
        return;
    }
  }

  private static void handlePost(final HttpServerExchange exchange) {
    checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
    this.dispatch(new Post(exchange));
  }

  private class Post implements Runnable {

    private final HttpServerExchange exchange;

    public Post(HttpServerExchange exchange) {
      checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
      this.exchange = exchange;
    }

    public void run() {
      try {
        final Map<String,Iterable<String>> params = this.exchange.getQueryParameters();
        final int limit = this.getFirstQueryParamInt("limit");
        final int offset = this.getFirstQueryParamInt("offset");
        this.exchange.getRequestReceiver().receiveFullString(new Receiver.FullStringCallback() {
          public void handle(final HttpServerExchange exchange, final String message) {
            LazyObject obj = new LazyObject(message);
            LazyObject[] sortings = obj.getArray("items");

          }
        });
      } catch(RuntimeException re) {
        throw re;
      } catch(Exception e) {
        throw new RuntimeException(e);
      }
    }

    private final int getFirstQueryParamInt(String key) {
      checkNotNull(key, "key to retrieve int from may not be null");
      final Map<String,Iterable<String>> params = this.exchange.getQueryParameters();
      checkNotNull(params, "Query parameters are null");
      final String values = params.get(key);
      checkNotNull(values, "values may not be null");
      final Iterable<String> iterable = this.exchange.getQueryParams();
      checkNotNull(iterable, "iterable for " + key + " may not be null");
      checkState(iterable.hasNext(), "no values for " + key + " in the query parameters");
      return Integer.parseInt(iterable.next());
    }

  }

  private static void handleGet(final HttpServerExchange exchange) {
    this.dispatch(new Get(exchange));
  }

  private class Get implements Runnable {

    private final HttpServerExchange exchange;

    private Get(final HttpServerExchange exchange) {
      checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
      this.exchange = exchange;
    }

    public void run() {
      try(final Connection conn = db.getConnection())  {
        try(final Statement stmt = conn.createStatement()) {
          try(final ResultSet rs = conn.executeQuery("SELECT COUNT(*) FROM usersview")) {
            checkState(rs.getNext(), "No results to our call");
            final int result = rs.getInt(1);
            exchange.getResponseSender().send(Integer.toString(result));
            exchange.endExchange();
          }
        }
      } catch(RuntimeException re) {
        throw re;
      } catch(Exception e) {
        throw new RuntimeException(e);
      }
    }
  }

  private static String getMethod(final HttpServerExchange exchange) {
    checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
    final HttpString methodHttpString = exchange.getRequestMethod();
    checkNotNull(methodHttpString, "Request doesn't have a method?");
    final String method = methodHttpString.toString();
    checkNotNull(method, "Method does not have a string representation");
    final String methodUpper = method.toUpper();
    return methodUpper;
  }

}
