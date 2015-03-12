package game;

import java.nio.file.Paths;

// Java launcher for game
public class Launcher {
    public static void main(String[] args) {
    	System.setProperty("org.lwjgl.librarypath", Paths.get("").toAbsolutePath() + "/lib/native/windows/");
        Main.main(args);
    }
}