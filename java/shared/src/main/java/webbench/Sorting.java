package webbench;

import java.util.Arrays;
import java.util.NoSuchElementException;
import static com.google.common.base.Preconditions.checkNotNull;

public class Sorting {

    public volatile Field field;
    public volatile Direction direction;

    public Sorting(String sortBy, String sortDir) {
        this.setSortBy(sortBy);
        this.setSortDir(sortDir);
    }

    public void setSortBy(String sortBy) {
        this.field = Field.fromJsonKey(sortBy);
    }

    public void setSortDir(String sortDir) {
        this.direction = Direction.fromJsonKey(sortDir);
    }

    public static String sortingsToSql(Sorting[] sortings) {
        checkNotNull(sortings, "sortings to convert to SQL may not be null");
        StringBuilder builder = new StringBuilder();
        Arrays.asList(sortings).forEach(sorting ->
            builder.append(sorting.field.sqlKey + " " + sorting.direction.sqlKey + ", ")
        );
        builder.append(" 1");
        return builder.toString();
    }

    public enum Direction {
        ASC("Ascending", "ASC"),
        DESC("Descending", "DESC");

        private final String jsonKey;
        private final String sqlKey;

        private Direction(String jsonKey, String sqlKey) {
            this.jsonKey = jsonKey;
            this.sqlKey = sqlKey;
        }

        public static Direction fromJsonKey(String field) {
            checkNotNull(field, "direction value may not be null");
            for(Direction value : values()) {
                if(value.jsonKey.equalsIgnoreCase(field)) {
                    return value;
                }
            }
            throw new NoSuchElementException("Could not find direction element matching '" + field + "'");
        }

        public String toSqlKey() {
            return sqlKey;
        }

    }

    public enum Field {
        STATE("State", "state"),
        LAST_NAME("LastName", "lastName"),
        AGE("Age", "age"),
        ZIPCODE("Zipcode", "zipcode"),
        CITY("City", "city"),
        FIRST_NAME("FirstName", "firstName");

        private final String jsonKey;
        private final String sqlKey;

        private Field(String jsonKey, String sqlKey) {
            this.jsonKey = jsonKey;
            this.sqlKey = sqlKey;
        }

        public static Field fromJsonKey(String field) {
            checkNotNull(field, "field value may not be null");
            for(Field value : values()) {
                if(value.jsonKey.equalsIgnoreCase(field)) {
                    return value;
                }
            }
            throw new NoSuchElementException("Could not find field element matching '" + field + "'");
        }

        public String toSqlKey() {
            return sqlKey;
        }

    }

}
