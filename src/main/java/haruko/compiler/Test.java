package haruko.compiler;


import org.objectweb.asm.ClassReader;
import org.objectweb.asm.util.TraceClassVisitor;

import java.io.InputStream;
import java.io.PrintWriter;

public class Test {
    private static Object x;
    public static void main(String[] args) throws Exception {
        x = 2;
        InputStream is = Test.class.getResourceAsStream(args[0]);
        ClassReader cr = new ClassReader(is);
        cr.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0);
    }
}
