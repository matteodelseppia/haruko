package haruko;

import haruko.compiler.Compiler;
import haruko.compiler.Parser;
import haruko.compiler.Lexer;

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
                Compiler compiler = new Compiler("Test" + i, parser.getProgramBody());
                byte[] code = compiler.getCode();
                HarukoClassLoader loader = new HarukoClassLoader();
                Method main = loader.defineClass("Test" + i, code).getMethod("main", String[].class);
                main.invoke(null, (Object) args);
            }
        } else {
            String source = new String(Files.readAllBytes(Paths.get(args[0])));
            String className = args[0].split("\\.")[0];
            System.out.println("compiling and running file " + args[0] + " with classname=" + className);
            Lexer lex = new Lexer(source);
            Parser parser = new Parser(lex.getTokens().iterator());
            Compiler compiler = new Compiler(className, parser.getProgramBody());
            byte[] code = compiler.getCode();
            HarukoClassLoader loader = new HarukoClassLoader();
            Method main = loader.defineClass(className, code).getMethod("main", String[].class);
            main.invoke(null, (Object) args);
        }
    }

    static class HarukoClassLoader extends ClassLoader {
        public Class defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}
