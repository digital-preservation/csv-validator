/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

import java.io.Reader;
import java.nio.charset.Charset;
import java.util.List;
import static uk.gov.nationalarchives.csv.validator.api.CsvValidator$.MODULE$;

/**
 * Validate that a csv file matches a format specified a csv schema.
 * This is a Java wrapper calling the main Scala application.
 *
 * <p> A typical invocation sequence:</p>
 * <blockquote><pre>{@code
 *  Boolean failFast = false;
 *  List<Substitution> pathSubstitutions = new ArrayList<Substitution>();
 *
 *  //add a substitution path
 *  pathSubstitutions.add(new Substitution("file://something", "/home/xxx"));
 *
 *  List<FailMessage> messages = CsvValidator.{@link #validate validate}(
 *    "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/data.csv",
 *    "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/data-schema.csvs",
 *    failFast,
 *    pathSubstitutions);
 *
 *  if(messages.isEmpty()) {
 *    System.out.println("All worked OK");
 *  } else {
 *    for(FailMessage message : messages) {
 *      if(message instanceof WarningMessage) {
 *        System.out.println("Warning: " + message.getMessage());
 *      } else {
 *        System.out.println("Error: " + message.getMessage());
 *      }
 *    }
 *  }
 * }</pre></blockquote>
 */
public class CsvValidator {

    /**
     * Validate a CSV file against a CSV schema file
     *
     * Assumes that the encoding of the CSV file and
     * CSV Schema file are both UTF-8
     *
     * @param csvFilename filename of the CSV file
     * @param csvSchemaFilename Filename of the CSV schema file
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final String csvFilename, final String csvSchemaFilename, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks) {
        return validate(csvFilename, MODULE$.DEFAULT_ENCODING(), csvSchemaFilename, MODULE$.DEFAULT_ENCODING(), failFast, pathSubstitutions, enforceCaseSensitivePathChecks);
    }

    /**
     * Validate a CSV file against a CSV schema file
     *
     * @param csvFilename filename of the CSV file
     * @param csvEncoding The charset encoding used in the CSV file
     * @param csvSchemaFilename Filename of the CSV schema file
     * @param csvSchemaEncoding The charset encoding used in the CSV Schema file
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final String csvFilename, final Charset csvEncoding, final String csvSchemaFilename, final Charset csvSchemaEncoding, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks) {
        return CsvValidatorJavaBridge.validate(csvFilename, csvEncoding, csvSchemaFilename, csvSchemaEncoding, failFast, pathSubstitutions, enforceCaseSensitivePathChecks);
    }

    /**
     * Validate a CSV file against a CSV schema file
     *
     * Assumes that the encoding of the CSV file and
     * CSV Schema file are both UTF-8
     *
     * @param csvFilename filename of the CSV file
     * @param csvSchemaFilename Filename of the CSV schema file
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param progress A callback to receive progress updates on the validation
     *                 process
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final String csvFilename, final String csvSchemaFilename, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final ProgressCallback progress) {
        return validate(csvFilename, MODULE$.DEFAULT_ENCODING(), csvSchemaFilename, MODULE$.DEFAULT_ENCODING(), failFast, pathSubstitutions, enforceCaseSensitivePathChecks, progress);
    }

    /**
     * Validate a CSV file against a CSV schema file
     *
     * @param csvFilename filename of the CSV file
     * @param csvEncoding The charset encoding used in the CSV file
     * @param csvSchemaFilename Filename of the CSV schema file
     * @param csvSchemaEncoding The charset encoding used in the CSV Schema file
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param progress A callback to receive progress updates on the validation
     *                 process
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final String csvFilename, final Charset csvEncoding, final String csvSchemaFilename, final Charset csvSchemaEncoding, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final ProgressCallback progress) {
        return CsvValidatorJavaBridge.validate(csvFilename, csvEncoding, csvSchemaFilename, csvSchemaEncoding, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, progress);
    }

    /**
     * Validate CSV data against a CSV schema
     *
     * @param csvData CSV data
     * @param csvSchema CSV schema
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final Reader csvData, final Reader csvSchema, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks) {
        return CsvValidatorJavaBridge.validate(csvData, csvSchema, failFast, pathSubstitutions, enforceCaseSensitivePathChecks);
    }

    /**
     * Validate CSV data against a CSV schema
     *
     * @param csvData CSV data
     * @param csvSchema CSV schema
     * @param failFast  true if you want to stop processing on the first error found,
     *                  false if you want to fine all errors
     * @param pathSubstitutions list of substitutions for file paths
     * @param progress A callback to receive progress updates on the validation
     *                 process
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    public static List<FailMessage> validate(final Reader csvData, final Reader csvSchema, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final ProgressCallback progress) {
        return CsvValidatorJavaBridge.validate(csvData, csvSchema, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, progress);
    }
}

