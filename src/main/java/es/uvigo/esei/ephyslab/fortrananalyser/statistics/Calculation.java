package es.uvigo.esei.ephyslab.fortrananalyser.statistics;

import org.apache.log4j.Logger;

import java.util.List;
import java.util.OptionalDouble;
import java.util.concurrent.TimeUnit;

public class Calculation {

    private static final Logger LOG = Logger.getLogger(Calculation.class.getName());

    public static Double calculateAverage(List<Double> l){
        OptionalDouble average = l.stream().mapToDouble(a -> a).average();
        return average.isPresent() ? average.getAsDouble() : 0.0;
    }

    public static String getDurationAnalyse(long millis) {
        if (millis < 0) {
            LOG.error(String.format("Milliseconds could not have negative value: %d", millis));
            throw new IllegalArgumentException("Duration must be greater than zero!");
        }
        long days = TimeUnit.MILLISECONDS.toDays(millis);
        millis -= TimeUnit.DAYS.toMillis(days);
        long hours = TimeUnit.MILLISECONDS.toHours(millis);
        millis -= TimeUnit.HOURS.toMillis(hours);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(millis);
        millis -= TimeUnit.MINUTES.toMillis(minutes);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(millis);
        millis -= TimeUnit.MILLISECONDS.toMillis(seconds);
        StringBuilder sb = new StringBuilder(64);
        sb.append(days);
        sb.append(" D ");
        sb.append(hours);
        sb.append(" h ");
        sb.append(minutes);
        sb.append(" min ");
        sb.append(seconds);
        sb.append(" s ");
        sb.append(millis);
        sb.append(" ms");
        return sb.toString();
    }
}
