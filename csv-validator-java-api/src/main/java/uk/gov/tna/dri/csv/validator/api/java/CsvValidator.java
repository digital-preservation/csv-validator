/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.api.java;

import java.util.List;

/**
* Validate that a cvs file matches a format specified by a uk.gov.tna.dri.csv.validator.schema.
* This is a Java wrapper calling the main Scala application.
*/
public class CsvValidator {

    /**
     * Check that a cvs formatted file matches a uk.gov.tna.dri.csv.validator.schema file.
     *
     * @param csvFilename Filename of cvs file
     * @param schemaFilename Filename of uk.gov.tna.dri.csv.validator.schema file
     * @param failFast  True if want to stop on first cvs file error
     * @param pathSubstitutions list of substitutions for file paths
     * @return empty list of no errors or list of error strings.
     */
    public static List<FailMessage> validate(final String csvFilename, final String schemaFilename, final boolean failFast, final List<Substitution> pathSubstitutions) {
        return CsvValidatorJavaBridge.validate(csvFilename, schemaFilename, failFast, pathSubstitutions);
    }

    /**
     * DEMO on how to use the java interface to call the validator
     *
     * @param args not used
     */
    /*
    public static void main(String[] args) {

        Boolean failFast = false;
        List<Substitution> pathSubstitutions = new ArrayList<Substitution>();

        // add a substitution path
//        pathSubstitutions.add( new Substitution( "file://something", "/home/xxx") );  // add a substitution path

        List<FailMessage> messages = CsvValidator.validate(
                "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/metaData.csv",
                "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/uk.gov.tna.dri.csv.validator.schema.txt",
                failFast,
                pathSubstitutions
        );

        if(messages.isEmpty()) {
            System.out.println( "All worked OK");
        } else {
            for (FailMessage message : messages) {
                if( message instanceof WarningMessage) {
                    System.out.println("Warning: " + message.getMessage());
                } else {
                    System.out.println("Error: " + message.getMessage());
                }
            }
        }
    }
    */
}

