package haruko.implementation;

public class Runtime {
    public static byte unboxBoolean(Boolean a) {
        if (a)
            return 1;
        return 0;
    }
}
