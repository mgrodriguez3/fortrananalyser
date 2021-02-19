package es.uvigo.esei.ephyslab.fortrananalyser.metric.comment;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class InitDocComment implements Runnable{

    private static final Logger LOG = Logger.getLogger(InitDocComment.class.getName());
    private static final String EXCLAMATION = "!";
    private final String filePath;
    private boolean initialComment;

    public InitDocComment(String filePath) {
        this.filePath = filePath;

    }

    @Override
    public void run() {
        try {
            String chain = "";
            int count = 0;
            int ite = 0;
            File file = new File(filePath);
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null && count < 2 && ite < 3) {
                    if (chain.contains(EXCLAMATION)) {
                        count++;
                    }
                    ite++;
                }
            }
            initialComment = (count > 1);
            LOG.info(String.format("{%s} - Initial comment analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isInitialComment() {
        return initialComment;
    }
}
