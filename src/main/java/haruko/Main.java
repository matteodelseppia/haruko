package haruko;

import haruko.implementation.compiler.Compiler;
import haruko.implementation.compiler.Parser;
import haruko.implementation.lexer.Lexer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Scanner scanner = new Scanner(System.in);
        int i = 0;
        while (true) {
            i++;
            System.out.print("> ");
            String source = scanner.nextLine();
            Lexer lex = new Lexer(source);
            Parser parser = new Parser(lex.getTokens().iterator());
            Compiler compiler = new Compiler("Test" + i, parser.getProgramBody());
            byte[] code = compiler.getCode();
            HarukoClassLoader loader = new HarukoClassLoader();
            Method main = loader.defineClass("Test" + i, code).getMethod("main", String[].class);
            main.invoke(null, (Object) args);
        }
    }

    static class HarukoClassLoader extends ClassLoader {
        public Class defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}
