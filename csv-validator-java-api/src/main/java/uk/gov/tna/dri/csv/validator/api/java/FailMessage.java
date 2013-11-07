package uk.gov.tna.dri.csv.validator.api.java;

public abstract class FailMessage {
    private final String message;

    public FailMessage(final String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}