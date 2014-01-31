package uk.gov.nationalarchives.csv.validator.api.java;

/**
 * Interface for receiving progress information
 */
public interface ProgressCallback {

    public void update(float complete);
}
