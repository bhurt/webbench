public class StringUtils {

    public static boolean isBlank(String s) {
       return (isEmpty(s) || isEmpty(s.trim()));
    }

    public static boolean isEmpty(String s) {
        return s == null || "".equals(s);
    }

    public static String defaultIfBlank(String value, String defult) {
        if(isBlank(value)) return defult;
        return value;
    }

}
