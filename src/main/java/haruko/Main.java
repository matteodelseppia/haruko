package haruko;

import haruko.implementation.compiler.Compiler;
import haruko.implementation.compiler.Parser;
import haruko.implementation.lexer.Lexer;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
        if (args[0].equals("--repl")) {
            Scanner scanner = new Scanner(System.in);
            int i = 0;
            while (true) {
                i++;
                System.out.print("> ");
                String source = scanner.nextLine();
                Lexer lex = new Lexer(source);
                Parser parser = new Parser(lex.getTokens().iterator());
                System.out.println(parser.getProgramBody());
                Compiler compiler = new Compiler("Test" + i, parser.getProgramBody());
                byte[] code = compiler.getCode();
                HarukoClassLoader loader = new HarukoClassLoader();
                Method main = loader.defineClass("Test" + i, code).getMethod("main", String[].class);
                main.invoke(null, (Object) args);
            }
        } else {
            String source = new String(Files.readAllBytes(Paths.get(args[0])));
            Lexer lex = new Lexer(source);
            Parser parser = new Parser(lex.getTokens().iterator());
            Compiler compiler = new Compiler("Test", parser.getProgramBody());
            byte[] code = compiler.getCode();
            HarukoClassLoader loader = new HarukoClassLoader();
            Method main = loader.defineClass("Test", code).getMethod("main", String[].class);
            main.invoke(null, (Object) args);
        }
    }

    static class HarukoClassLoader extends ClassLoader {
        public Class defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}
