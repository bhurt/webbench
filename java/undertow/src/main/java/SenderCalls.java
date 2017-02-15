import io.undertow.io.IoCallback;
import io.undertow.io.Sender;
import io.undertow.server.HttpServerExchange;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.google.common.base.Preconditions.checkNotNull;

public class SenderCalls implements AutoCloseable {

    private final BlockingQueue<String> values = new ArrayBlockingQueue<>(10, true);
    private final AtomicBoolean stop = new AtomicBoolean(false);

    public SenderCalls(HttpServerExchange exchange) {
        checkNotNull(exchange, "server exchange may not be null");
        this.runSendThread(exchange.getResponseSender());
    }

    public void send(String value) {
        checkNotNull(value, "value to send may not be null");
        if(stop.get()) {
            throw new IllegalStateException("Sender calls was already closed but still added a new value");
        }
        values.add(value);
    }

    private void runSendThread(final Sender sender) {
        ForkJoinPool.commonPool().submit(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final List<String> strings;
                if(values.isEmpty()) {
                    final String value = values.poll(1L, TimeUnit.MILLISECONDS);
                    if(value == null) {
                        if(!stop.get()) {
                            runSendThread(sender);
                        }
                        return null;
                    }
                    strings = Collections.singletonList(value);
                } else {
                   strings = new ArrayList<>(values.size() * 2);
                   values.drainTo(strings);
                }
                String value = join(strings);
                sender.send(value, new IoCallback() {
                    @Override
                    public void onComplete(HttpServerExchange exchange, Sender sender) {
                        if(values.isEmpty() && stop.get()) {
                            exchange.endExchange();
                            return;
                        } else {
                            runSendThread(sender);
                        }
                    }

                    @Override
                    public void onException(HttpServerExchange exchange, Sender sender, IOException exception) {
                        stop.set(true);
                        exception.printStackTrace();
                        exchange.setStatusCode(500);
                        exchange.setReasonPhrase(StringUtils.defaultIfBlank(exception.getMessage(), exception.getClass().getSimpleName()));
                        exchange.endExchange();
                    }
                });

                return null;
            }

            private String join(List<String> strings) {
                checkNotNull(strings, "strings to join may not be null");
                switch(strings.size()) {
                    case 0:
                        return "";
                    case 1:
                        return strings.get(0);
                    case 2:
                        return strings.get(0) + strings.get(1);
                    default:
                        final StringBuilder builder = new StringBuilder();
                        strings.forEach(str -> builder.append(str));
                        return builder.toString();
                }
            }
        });
    }

    @Override
    public void close() throws Exception {
        stop.set(true);
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            close();
        } finally {
            super.finalize();
        }
    }
}
