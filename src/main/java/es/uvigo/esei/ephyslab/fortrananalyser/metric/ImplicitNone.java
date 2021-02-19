package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ImplicitNone implements Runnable {

    private static final Logger LOG = Logger.getLogger(ImplicitNone.class.getName());
    private static final String IMPLICIT_NONE = "IMPLICIT NONE";
    private static final String EXCLAMATION = "!";
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
                    if (!chain.contains(EXCLAMATION) && chain.contains(IMPLICIT_NONE)) {
                        useImplicitNone = true;
                        break;
                    }
                }
            }
            LOG.info(String.format("{%s} - Implicit none sentence analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isUseImplicitNone() {
        return useImplicitNone;
    }
}
