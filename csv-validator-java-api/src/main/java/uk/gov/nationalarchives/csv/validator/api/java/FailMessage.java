/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

/**
 * Abstract type for CSV Schema Parser and CSV Validation errors
 */
public abstract class FailMessage {
    private final String message;

    /**
     * @param message The failure message
     */
    public FailMessage(final String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}