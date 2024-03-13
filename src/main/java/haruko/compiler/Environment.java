package haruko.compiler;

public class Environment {
    public int max_local_id;

    public Environment(int max_local_id) {
        this.max_local_id = max_local_id;
    }
}
