package haruko.implementation.compiler;


import haruko.implementation.lexer.Lexer;
import haruko.lang.Core;
import haruko.lang.Core$;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.TraceClassVisitor;
import scala.collection.Seq;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;

public class Test {
    private static Object x;
    public static void main(String[] args) throws Exception {
        var x = 2;
        var is = Test.class.getResourceAsStream(args[0]);
        var cr = new ClassReader(is);
        cr.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0);
    }
}
