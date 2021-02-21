package es.uvigo.esei.ephyslab.fortrananalyser.util;

import es.uvigo.esei.ephyslab.fortrananalyser.TasksBar;

import java.io.File;
import java.nio.file.Paths;
import java.util.List;

public class FileUtils {
    private static final String REPORT_NAME = System.getProperty("user.home") + "/temp/QualityReport.pdf";
    private static final String REPORT_PATH = System.getProperty("user.home") + "/temp";

    public static void scanFilesInDirectory(String directoryName, List<File> files) {
        File directory = new File(directoryName);
        File[] fList = directory.listFiles();
        if (fList != null) {
            for (File file : fList) {
                if (file.isFile()) {
                    files.add(file);
                } else if (file.isDirectory()) {
                    scanFilesInDirectory(file.getAbsolutePath(), files);
                }
            }
        }
    }

    public static String getPathFromFile(File file) {
        return file.getAbsolutePath().
                substring(0, file.getAbsolutePath().lastIndexOf(File.separator));

    }

    public static String getFileExtension(File file) {
        String name = file.getName();
        try {
            return name.substring(name.lastIndexOf('.') + 1);
        } catch (Exception e) {
            return "";
        }
    }

    public static void checkTempFileExist() {
        if (!Paths.get(REPORT_NAME).toFile().exists()) {
            new File(REPORT_PATH).mkdirs();
        }
    }
}
