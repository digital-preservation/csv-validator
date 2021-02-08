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
 * Error Message (failure)
 */
public class ErrorMessage extends FailMessage {

    public ErrorMessage(final String message, final int lineNumber, final int columnIndex) {
        super(message, lineNumber, columnIndex);
    }
    public ErrorMessage(final String message, final int lineNumber, final int columnIndex,final String resourceTag) {
        super(message, lineNumber, columnIndex,resourceTag);
    }
}
