import me.doubledutch.lazyjson.LazyArray;
import me.doubledutch.lazyjson.LazyObject;

import java.util.*;
import static com.google.common.base.Preconditions.checkNotNull;

public class UserSort {

    public final int offset;
    public final int limit;
    public final Sorting[] sortings;

    public UserSort(Map<String, Deque<String>> queryParams, String jsonBody) {
        checkNotNull(queryParams, "query params may not be null");
        checkNotNull(jsonBody, "json body may not be null");
        this.limit = readQueryInt(queryParams, "limit");
        this.offset = readQueryInt(queryParams, "offset");
        this.sortings = readSortings(jsonBody);
    }

    private static Sorting[] readSortings(final String jsonBody) {
        LazyObject obj = new LazyObject(jsonBody);
        LazyArray sortingsJson = obj.getJSONArray("items");
        checkNotNull(sortingsJson, "Could not find 'items': " + obj.toString());
        final int sortingsLength = sortingsJson.length();
        final Sorting[] sortings = new Sorting[sortingsLength];
        for(int i = 0; i < sortingsLength; i++) {
            final LazyObject sortingObj = sortingsJson.getJSONObject(i);
            final String field = sortingObj.getString("sortBy");
            final String direction = sortingObj.getString("sortDir");
            sortings[i] = new Sorting(field, direction);
        }
        return  sortings;
    }

    private static int readQueryInt(final Map<String,Deque<String>> params, final String key) {
        String value = params.get(key).getFirst();
        checkNotNull(value, "no value found for " + key);
        return Integer.parseInt(value);
    }


}
