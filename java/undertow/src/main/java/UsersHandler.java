import io.undertow.io.Receiver;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;
import me.doubledutch.lazyjson.LazyArray;
import me.doubledutch.lazyjson.LazyObject;

import javax.sql.DataSource;

import java.util.Deque;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;

public class UsersHandler implements HttpHandler {

  private final DataSource db;

  public UsersHandler(DataSource ds) {
    checkNotNull(ds, "DataSource may not be null");
    this.db = ds;
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

  private void handlePost(final HttpServerExchange exchange) {
    checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
    exchange.dispatch(new Post(exchange));
  }

  private class Post implements Runnable {

    private final HttpServerExchange exchange;

    public Post(HttpServerExchange exchange) {
      checkNotNull(exchange, "Undertow told us that the exchange object would never be null!");
      this.exchange = exchange;
    }

    public void run() {
      try {
        final int limit = this.getFirstQueryParamInt("limit");
        final int offset = this.getFirstQueryParamInt("offset");
        this.exchange.getRequestReceiver().receiveFullString((exchange, message) -> {
          LazyObject obj = new LazyObject(message);
          LazyArray sortingsJson = obj.getJSONArray("items");
          checkNotNull(sortingsJson, "Could not find 'items': " + obj.toString());
          final int sortingsLength = sortingsJson.length();
          Sorting[] sortings = new Sorting[sortingsLength];
          for(int i = 0; i < sortingsLength; i++) {

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
      final Map<String,Deque<String>> params = this.exchange.getQueryParameters();
      checkNotNull(params, "Query parameters are null");
      final Deque<String> values = params.get(key);
      checkNotNull(values, "values may not be null");
      return Integer.parseInt(values.getFirst());
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
