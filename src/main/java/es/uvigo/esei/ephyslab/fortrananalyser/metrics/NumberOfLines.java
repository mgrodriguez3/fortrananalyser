package es.uvigo.esei.ephyslab.fortrananalyser.metrics;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class NumberOfLines implements Runnable {

    private final String filePath;
    private int numOfLines;

    public NumberOfLines(String filePath) {
        this.filePath = filePath;
        numOfLines = 0;
    }

    @Override
    public void run() {
        try {
            String line = "";
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((line = b.readLine()) != null) {
                    numOfLines++;
                }
            }
        } catch (IOException e) {
            System.out.println("ERROR");
        }
    }

    public int getNumOfLines() {
        return numOfLines;
    }
}
