package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ExitUse implements Runnable {

    private static final Logger LOG = Logger.getLogger(ExitUse.class.getName());
    private static final String END_DO = "END DO";
    private static final String DO = "DO";
    private static final String EXCLAMATION = "!";
    private static final String EXIT = "EXIT";
    private final String filePath;
    private boolean useExit = false;

    public ExitUse(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            File file = new File(filePath);
            int numLoops = 0;
            int numExit = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains("!")
                            && !chain.contains(END_DO)
                            && chain.contains(DO)) {
                        numLoops++;
                    }
                    if ((chain.contains(EXIT))
                            && !chain.contains(EXCLAMATION)) {
                        numExit++;
                    }
                }
            }
            useExit = (numLoops == numExit);
            LOG.info(String.format("{%s} - Exit sentence  use analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isUseExit() {
        return useExit;
    }
}
