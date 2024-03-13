package haruko.implementation;

public class JRuntime {
    public static byte unboxBoolean(Boolean a) {
        if (a)
            return 1;
        return 0;
    }
}
