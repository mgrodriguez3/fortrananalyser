package es.uvigo.esei.ephyslab.fortrananalyser.metrics;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ImplicitNone implements Runnable {

    private boolean useImplicitNone = false;
    private final String filePath;

    public ImplicitNone(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains("!") && chain.contains("IMPLICIT NONE")) {
                        useImplicitNone = true;
                        break;
                    }
                }
            }
        } catch (IOException e) {
            System.out.println("ERROR");
        }
    }
}
