/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.api.java;

/**
 * Path substitutions are used where part of a
 * file path is to be replaced by something else
 *
 * Useful for when CSV files have hard-coded absolute paths
 * to files that accompany the CSV file
 */
public class Substitution {
    private final String from;
    private final String to;

    /**
     * Path Substitution
     *
     * @param from Part of a path to replace
     * @param to The replacement to place into the path
     */
    public Substitution(final String from, final String to) {
        this.from = from;
        this.to = to;
    }

    /**
     * Substitution from
     */
    public String getFrom() {
        return from;
    }

    /**
     * Substitution to
     */
    public String getTo() {
        return to;
    }
}
