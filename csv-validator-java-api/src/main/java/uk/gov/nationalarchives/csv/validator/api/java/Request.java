/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

import java.util.ArrayList;

public interface Request {
    boolean failFast = false;
    ArrayList<Substitution> pathSubstitutionsList = new ArrayList<>();
    boolean enforceCaseSensitivePathChecks = false;
    boolean trace = false;
    ProgressCallback progress = null;
    boolean skipFileChecks = false;
    int maxCharsPerCellLimit = 4096;
}
