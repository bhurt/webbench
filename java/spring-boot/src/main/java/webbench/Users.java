package webbench;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

@RestController
public class Users {

    private final JdbcTemplate jdbc;

    @Autowired
    public Users(final JdbcTemplate jdbc) {
        checkNotNull(jdbc, JdbcTemplate.class.getName() + " argument is null");
        this.jdbc = jdbc;
    }


    @RequestMapping(path="/rest/v1/users", method = RequestMethod.GET)
    public int get() {
        return jdbc.query("SELECT COUNT(*) AS \"cnt\" FROM userView", new ResultSetExtractor<Integer>() {
            @Override
            public Integer extractData(ResultSet rs) throws SQLException, DataAccessException {
                rs.next();
                return rs.getInt("cnt");
            }
        });
    }

    @RequestMapping(path="/rest/v1/users", method = RequestMethod.POST)
    public List<User> post(
        @RequestParam("limit") int limit,
        @RequestParam("offset") int offset,
        SortBody body
    ) {
        String sortingString = Sorting.sortingsToSql(body.items);
        final String sql =
                new StringBuilder("SELECT * FROM userView ")
                .append(" ORDER BY ").append(sortingString).append(' ')
                .append(" LIMIT ").append(limit).append(' ')
                .append(" OFFSET ").append(offset).append(' ')
                .toString();
        return jdbc.query(sql, new RowMapper<User>() {
            public User mapRow(ResultSet rs, int rowNum) throws SQLException {
                try {
                    return UserReader.toUser(rs);
                } catch(RuntimeException re) {
                    throw re;
                } catch(SQLException sqle) {
                    throw sqle;
                } catch(Exception e) {
                    throw new RuntimeException(e);
                }
            }
        });
    }


}
