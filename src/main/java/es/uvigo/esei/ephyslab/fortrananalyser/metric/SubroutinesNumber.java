package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class SubroutinesNumber implements Runnable {

    private static final String SUBROUTINE = "SUBROUTINE";
    private static final String END_SUBROUTINE = "END SUBROUTINE";
    private static final String EXCLAMATION = "!";
    private static final Logger LOG = Logger.getLogger(SubroutinesNumber.class.getName());
    private final String filePath;
    private int numSubroutines;

    public SubroutinesNumber(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            numSubroutines = 0;
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (chain.contains(SUBROUTINE)
                            && !chain.contains(EXCLAMATION)
                            && chain.contains(END_SUBROUTINE)) {
                        numSubroutines++;
                    }
                }
            }
            LOG.info(String.format("{%s} - Subroutines number analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public int getNumSubroutines() {
        return numSubroutines;
    }
}
