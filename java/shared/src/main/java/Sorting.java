import java.util.NoSuchElementException;

public class Sorting {

    public final Field field;
    public final Direction direction;

    public Sorting(String field, String direction) {
        this.field = Field.fromJsonKey(field);
        this.direction = Direction.fromJsonKey(direction);
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
            if(StringUtils.isBlank(field)) return null;
            for(Direction value : values()) {
                if(value.jsonKey.equalsIgnoreCase(field)) {
                    return value;
                }
            }
            throw new NoSuchElementException("Could not find element matching '" + field + "'");
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
            if(StringUtils.isBlank(field)) return null;
            for(Field value : values()) {
                if(value.jsonKey.equalsIgnoreCase(field)) {
                    return value;
                }
            }
            throw new NoSuchElementException("Could not find element matching '" + field + "'");
        }

        public String toSqlKey() {
            return sqlKey;
        }

    }

}
