import io.undertow.io.IoCallback;
import io.undertow.io.Receiver;
import io.undertow.io.Sender;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;
import me.doubledutch.lazyjson.LazyArray;
import me.doubledutch.lazyjson.LazyObject;

import javax.sql.DataSource;

import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Deque;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;

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
        this.exchange.getRequestReceiver().receiveFullString((exchange, message) -> {
          final UserSort sorting = new UserSort(exchange.getQueryParameters(), message);
          try(final Connection conn = db.getConnection()) {
            try(final Statement stmt = conn.createStatement()) {
              final String sql =
                new StringBuilder("SELECT * FROM userView ")
                  .append("ORDER BY ").append(Sorting.sortingsToSql(sorting.sortings)).append(' ')
                  .append("LIMIT ").append(sorting.limit).append(' ')
                  .append("OFFSET ").append(sorting.offset).append(' ')
                  .toString();
              try(final ResultSet rs = stmt.executeQuery(sql)) {
                try(SenderCalls sender = new SenderCalls(exchange)) {
                  sender.send("[");
                  while (rs.next()) {
                    String json = UserReader.toJson(rs);
                    sender.send(json);
                    if (!rs.isLast()) {
                      sender.send(",");
                    }
                  }
                  sender.send("]");
                }
              }
            }
          } catch(RuntimeException re) {
            throw re;
          } catch(Exception e) {
            throw new RuntimeException(e);
          }
        });
      } catch(RuntimeException re) {
        throw re;
      } catch(Exception e) {
        throw new RuntimeException(e);
      }
    }

  }

  private void handleGet(final HttpServerExchange exchange) {
    exchange.dispatch(new Get(exchange));
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
          try(final ResultSet rs = stmt.executeQuery("SELECT COUNT(*) AS 'cnt' FROM userView")) {
            checkState(rs.next(), "No results to our call");
            final int result = rs.getInt("cnt");
            exchange.getResponseSender().send(Integer.toString(result));
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
    final String methodUpper = method.toUpperCase();
    return methodUpper;
  }

}
