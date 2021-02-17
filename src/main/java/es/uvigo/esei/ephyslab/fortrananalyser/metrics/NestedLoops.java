package es.uvigo.esei.ephyslab.fortrananalyser.metrics;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class NestedLoops implements Runnable {
    private static final String END_DO = "END DO";
    private final String filePath;
    private boolean useNestedLoops = false;

    public NestedLoops(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            int nestedLoops = 0;
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains("!")
                            && !chain.contains(END_DO)
                            && chain.contains("DO")) {
                        nestedLoops++;
                    }
                    if (!chain.contains("!")
                            && chain.contains(END_DO)) {
                        nestedLoops--;
                    }
                }
            }
            useNestedLoops = (nestedLoops >= 0 && nestedLoops <= 3 );
        } catch (IOException e) {
            System.out.println("ERROR");
        }
    }

    public boolean isUseNestedLoops() {
        return useNestedLoops;
    }
}
