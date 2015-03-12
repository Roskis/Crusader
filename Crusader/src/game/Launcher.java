package game;

import java.nio.file.Paths;

// Java launcher for game
public class Launcher {
    public static void main(String[] args) {
    	if (System.getProperty("os.name").contains("Windows")) {
    		System.setProperty("org.lwjgl.librarypath", Paths.get("").toAbsolutePath() + "/lib/native/windows/");
    	} else if (System.getProperty("os.name").contains("Mac")) {
    		System.setProperty("org.lwjgl.librarypath", Paths.get("").toAbsolutePath() + "/lib/native/macosx/");
    	} else if (System.getProperty("os.name").contains("Linux")) {
    		System.setProperty("org.lwjgl.librarypath", Paths.get("").toAbsolutePath() + "/lib/native/linux/");
    	} else {
    		throw new RuntimeException("Your OS is not supported");
    	}
        Main.main(args);
    }
}