package webbench;

import org.boon.json.JsonFactory;
import org.boon.json.JsonSerializer;

import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;

public class UserReader {

    private static final String[] EMPTY_STRING_ARRAY = new String[0];
    private static final JsonSerializer JSON = JsonFactory.create().serializer();

    public static List<User> toUsers(ResultSet rs) throws Exception {
        checkNotNull(rs, "result set to read from may not be null");
        checkState(!rs.isClosed(), "result set to be read from is closed");
        checkState(!rs.isAfterLast(), "result set to be read from is after the last record");

        List<User> users = new ArrayList<>();
        while(rs.next()) {
            users.add(toUser(rs));
        }
        return users;
    }

    public static User toUser(ResultSet rs) throws Exception {
        checkNotNull(rs, "result set to read from may not be null");
        checkState(!rs.isClosed(), "result set to be read from is closed");
        checkState(!rs.isAfterLast(), "result set to be read from is after the last record");

        final User user = new User();
        user.id = rs.getInt("id");
        user.firstName = rs.getString("firstName");
        user.middleName = rs.getString("middleName");
        user.lastName = rs.getString("lastName");
        user.title = rs.getString("title");
        user.streetAddress = rs.getString("streetAddress");
        user.city = rs.getString("city");
        user.state = rs.getString("state");
        user.zipcode = rs.getString("zipcode");
        user.phoneNumber = rs.getString("phoneNumber");
        user.age = rs.getInt("age");
        user.interests = readInterests(rs.getArray("interests").getResultSet());

        return user;
    }

    public static String toUserJson(ResultSet rs) throws Exception {
        checkNotNull(rs, "result set to read from may not be null");
        checkState(!rs.isClosed(), "result set to be read from is closed");
        checkState(!rs.isAfterLast(), "result set to be read from is after the last record");

        User user = toUser(rs);

        return JSON.serialize(user).toStringAndRecycle();
    }

    private static String[] readInterests(ResultSet rs) throws Exception {
        checkNotNull(rs, "result set to read from may not be null");
        checkState(!rs.isClosed(), "result set to be read from is closed");

        if(rs.isAfterLast()) return EMPTY_STRING_ARRAY; // No interests

        final List<String> interests = new ArrayList<>(6);
        while(rs.next()) {
            interests.add(rs.getString(1));
        }
        return interests.toArray(EMPTY_STRING_ARRAY);
    }

}
