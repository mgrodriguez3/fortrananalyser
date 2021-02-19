package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class NestedLoops implements Runnable {

    private static final Logger LOG = Logger.getLogger(NestedLoops.class.getName());
    private static final String END_DO = "END DO";
    private static final String DO = "DO";
    private static final String EXCLAMATION = "!";
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
                    if (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_DO)
                            && chain.contains(DO)) {
                        nestedLoops++;
                    }
                    if (!chain.contains(EXCLAMATION)
                            && chain.contains(END_DO)) {
                        nestedLoops--;
                    }
                }
            }
            useNestedLoops = (nestedLoops >= 0 && nestedLoops <= 3);
            LOG.info(String.format("{%s} - Nested loops analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isUseNestedLoops() {
        return useNestedLoops;
    }
}
