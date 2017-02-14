import java.util.function.Function;

public class EnvUtils {

    public static String getEnv(String key, String defult) {
        return StringUtils.defaultIfBlank(System.getenv(key), defult);
    }

    public static <T> T getEnv(String key, T defult, Function<String,T> conversion) {
        String value = System.getenv(key);
        if(StringUtils.isBlank(value)) return defult;
        return conversion.apply(value);
    }
}
