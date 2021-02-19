package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class FunctionsNumber implements Runnable {

    private static final Logger LOG = Logger.getLogger(FunctionsNumber.class.getName());
    private static final String END_FUNCTION = "END FUNCTION ";
    private static final String FUNCTION = "FUNCTION ";
    private static final String EXCLAMATION = "!";
    private final String filePath;
    private int numFuncions;

    public FunctionsNumber(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            numFuncions = 0;
            String chain = "";
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_FUNCTION)
                            && chain.contains(FUNCTION)) {
                        numFuncions++;
                    }
                }
            }
            LOG.info(String.format("{%s} - Functions number analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public int getNumFuncions() {
        return numFuncions;
    }
}
