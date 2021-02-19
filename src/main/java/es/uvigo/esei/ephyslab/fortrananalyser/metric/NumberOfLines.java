package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class NumberOfLines implements Runnable {

    private static final Logger LOG = Logger.getLogger(NumberOfLines.class.getName());
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
            LOG.info(String.format("{%s} - Number of lines analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public int getNumOfLines() {
        return numOfLines;
    }
}
