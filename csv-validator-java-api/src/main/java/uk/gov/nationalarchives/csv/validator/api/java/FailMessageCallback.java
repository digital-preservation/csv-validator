/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

/**
 * Receives the errors and warnings of a validation as soon as they are found,
 * instead of all of them at the end, so that a large CSV file with many
 * failing rows can be validated without holding every message in memory.
 */
public interface FailMessageCallback {
    void onFailMessage(FailMessage message);
}
