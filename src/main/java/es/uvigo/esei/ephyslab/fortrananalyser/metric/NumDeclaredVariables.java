package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class NumDeclaredVariables implements Runnable {

    private static final Logger LOG = Logger.getLogger(NumDeclaredVariables.class.getName());
    private static final String CONSTRUCTOR = "::";
    private static final String COMA = ",";
    private final String filePath;
    private int numVariables;

    public NumDeclaredVariables(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {

        try {
            String chain = "";
            numVariables = 0;
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    if (chain.contains(CONSTRUCTOR)) {
                        numVariables++;
                        if (chain.contains(COMA) && chain.indexOf(CONSTRUCTOR) <= chain.indexOf(',')) {
                            numVariables++;
                        }
                    }
                }
            }
            LOG.info(String.format("{%s} - Implicit none sentence analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public int getNumVariables() {
        return numVariables;
    }
}

