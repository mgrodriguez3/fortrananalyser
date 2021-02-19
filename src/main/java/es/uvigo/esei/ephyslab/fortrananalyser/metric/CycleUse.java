package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class CycleUse implements Runnable {

    private static final Logger LOG = Logger.getLogger(CycleUse.class.getName());
    private static final String END_DO = "END DO";
    private static final String DO = "DO";
    private static final String EXCLAMATION = "!";
    private static final String CYCLE = "CYCLE";
    private final String filePath;
    private boolean useCycle = false;

    public CycleUse(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            File file = new File(filePath);
            int numCycles = 0;
            int numLoops = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_DO)
                            && chain.contains(DO)) {
                        numLoops++;
                    }
                    if (chain.contains(CYCLE)
                            && !chain.contains(EXCLAMATION)) {
                        numCycles++;
                    }
                }
            }
            useCycle = (numLoops == numCycles);
            LOG.info(String.format("{%s} - Cycle sentence use analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isUseCycle() {
        return useCycle;
    }
}
