package uk.gov.tna.dri.csv.validator.api.java;

/**
 * A path substitution line for file access where cvs given paths need to be replaced.
 */
public class Substitution {
    private final String from;
    private final String to;

    public Substitution(final String from, final String to) {
        this.from = from;
        this.to = to;
    }

    public String getFrom() {
        return from;
    }

    public String getTo() {
        return to;
    }
}
