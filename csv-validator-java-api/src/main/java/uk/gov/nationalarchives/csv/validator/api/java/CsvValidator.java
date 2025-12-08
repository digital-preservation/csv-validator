/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

import java.io.Reader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import static uk.gov.nationalarchives.csv.validator.api.CsvValidator$.MODULE$;

/**
 * Validate that a csv file matches a format specified a csv schema.
 * This is a Java wrapper calling the main Scala application.
 *
 * <p> A typical invocation sequence:</p>
 * <blockquote><pre>{@code
 *  Charset csvEncoding = JCharset.forName("UTF-8"); // default is UTF-8
 *  Charset csvSchemaEncoding = JCharset.forName("UTF-8"); // default is UTF-8
 *  boolean failFast = true; // default is false
 *  List<Substitution> pathSubstitutions = new ArrayList<Substitution>(); // default is any empty ArrayList
 *  boolean enforceCaseSensitivePathChecks = true; // default is false
 *  boolean trace = false; // default is false
 *  ProgressCallback progress; // default is null
 *  boolean skipFileChecks = true; // default is false
 *  int maxCharsPerCell = 8096; // default is 4096
 *
 *  //add a substitution path
 *  pathSubstitutions.add(new Substitution("file://something", "/home/xxx"));
 *
 *  CsvValidator.ValidatorBuilder validateWithStringNames = new CsvValidator.ValidatorBuilder(
 *      "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/data.csv",
 *      "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/data-schema.csvs"
 *  )
 *
 *  // alternatively, you can pass in Readers for each file
 *  Reader csvReader = new Reader();
 *  Reader csvSchemaReader = new Reader();
 *  CsvValidator.ValidatorBuilder validateWithReaders = new CsvValidator.ValidatorBuilder(
 *      csvReader, csvSchemaReader
 *  )
 *
 *  List<FailMessage> messages = validateWithStringNames
 *    .csvEncoding()
 *    .csvSchemaEncoding()
 *    .failFast(failFast)
 *    .pathSubstitutions(pathSubstitutions)
 *    .enforceCaseSensitivePathChecks(enforceCaseSensitivePathChecks)
 *    .trace(trace)
 *    .progress(progress)
 *    .skipFileChecks(skipFileChecks)
 *    .maxCharsPerCell(maxCharsPerCell)
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
     * @param trace When set to true, a trace of the grammar parse will be output
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final String csvFilename, final String csvSchemaFilename, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace) {
        return validate(csvFilename, MODULE$.DEFAULT_ENCODING(), csvSchemaFilename, MODULE$.DEFAULT_ENCODING(), failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace);
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
     * @param trace When set to true, a trace of the grammar parse will be output
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final String csvFilename, final Charset csvEncoding, final String csvSchemaFilename, final Charset csvSchemaEncoding, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace) {
        return CsvValidatorJavaBridge.validate(csvFilename, csvEncoding, csvSchemaFilename, csvSchemaEncoding, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace);
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
     * @param enforceCaseSensitivePathChecks Enforces case-sensitive file path checks
     * @param trace When set to true, a trace of the grammar parse will be output
     * @param progress A callback to receive progress updates on the validation
     *                 process
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final String csvFilename, final String csvSchemaFilename, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace, final ProgressCallback progress) {
        return validate(csvFilename, MODULE$.DEFAULT_ENCODING(), csvSchemaFilename, MODULE$.DEFAULT_ENCODING(), failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace, progress);
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
     * @param trace When set to true, a trace of the grammar parse will be output
     * @param progress A callback to receive progress updates on the validation
     *                 process
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final String csvFilename, final Charset csvEncoding, final String csvSchemaFilename, final Charset csvSchemaEncoding, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace, final ProgressCallback progress) {
        return CsvValidatorJavaBridge.validate(csvFilename, csvEncoding, csvSchemaFilename, csvSchemaEncoding, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace, progress);
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
     * @param trace When set to true, a trace of the grammar parse will be output
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final Reader csvData, final Reader csvSchema, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace) {
        return CsvValidatorJavaBridge.validate(csvData, csvSchema, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace);
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
     * @param trace When set to true, a trace of the grammar parse will be output
     * @param progress A callback to receive progress updates on the validation
     *                 process
     *
     * @return empty list of (if there are no errors), or list of error strings.
     */
    @Deprecated
    public static List<FailMessage> validate(final Reader csvData, final Reader csvSchema, final boolean failFast, final List<Substitution> pathSubstitutions, final Boolean enforceCaseSensitivePathChecks, final Boolean trace, final ProgressCallback progress) {
        return CsvValidatorJavaBridge.validate(csvData, csvSchema, failFast, pathSubstitutions, enforceCaseSensitivePathChecks, trace, progress);
    }

    public static class ValidatorBuilder {
        private String csvFileName;
        private String csvSchemaFilename;
        private Reader csvReader;
        private Reader csvSchemaReader;
        private Charset csvEncoding = MODULE$.DEFAULT_ENCODING();
        private boolean validateUtf8Encoding = csvEncoding.name().equals("UTF-8");
        private Charset csvSchemaEncoding = MODULE$.DEFAULT_ENCODING();
        private boolean failFast = false;
        private List<Substitution> pathSubstitutions = new ArrayList<>();
        private boolean enforceCaseSensitivePathChecks = false;
        private boolean trace = false;
        private ProgressCallback progress;
        private boolean skipFileChecks = false;
        private int maxCharsPerCell = 4096;

        private boolean textFileValidation = false;

        public ValidatorBuilder(String csvFileName, String csvSchemaFilename) {
            this.csvFileName = csvFileName;
            this.csvSchemaFilename = csvSchemaFilename;
            this.textFileValidation = true;
        }

        public ValidatorBuilder(Reader csvReader, Reader csvSchemaReader) {
            this.csvReader = csvReader;
            this.csvSchemaReader = csvSchemaReader;
        }

        public ValidatorBuilder usingCsvEncoding(Charset encoding, boolean validateUtf8Encoding) throws Exception {
            if(!encoding.name().equals("UTF-8") && validateUtf8Encoding){
                throw new Exception("'validateUtf8Encoding' is set to 'true' but " + encoding.name() + " charset was passed in");
            }

            this.csvEncoding = encoding;
            this.validateUtf8Encoding = validateUtf8Encoding;
            return this;
        }

        public ValidatorBuilder usingCsvSchemaEncoding(Charset schemaEncoding) {
            this.csvSchemaEncoding = schemaEncoding;
            return this;
        }

        public ValidatorBuilder usingFailFast(boolean failFast) {
            this.failFast = failFast;
            return this;
        }

        public ValidatorBuilder usingPathSubstitutions(List<Substitution> pathSubstitutions) {
            this.pathSubstitutions = pathSubstitutions;
            return this;
        }

        public ValidatorBuilder usingEnforceCaseSensitivePathChecks(boolean enforceCaseSensitivePathChecks) {
            this.enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecks;
            return this;
        }

        public ValidatorBuilder usingTrace(boolean trace) {
            this.trace = trace;
            return this;
        }

        public ValidatorBuilder usingProgress(ProgressCallback progress) {
            this.progress = progress;
            return this;
        }

        public ValidatorBuilder usingSkipFileChecks(boolean skipFileChecks) {
            this.skipFileChecks = skipFileChecks;
            return this;
        }

        public ValidatorBuilder usingMaxCharsPerCell(int maxCharsPerCell) {
            this.maxCharsPerCell = maxCharsPerCell;
            return this;
        }

        public Result runValidation() {
            if(textFileValidation) {
                return CsvValidatorJavaBridge.validate(new CsvValidatorJavaBridge.ValidationRequest(this.csvFileName, this.csvEncoding, this.validateUtf8Encoding, this.csvSchemaFilename, this.csvSchemaEncoding, true,  this.failFast, this.pathSubstitutions, this.enforceCaseSensitivePathChecks, this.trace, this.progress, this.skipFileChecks, this.maxCharsPerCell));
            } else {
                return CsvValidatorJavaBridge.validate(new CsvValidatorJavaBridge.ReaderValidationRequest(this.csvReader, this.csvSchemaReader, this.failFast, this.pathSubstitutions, this.enforceCaseSensitivePathChecks, this.trace, this.progress, this.skipFileChecks, this.maxCharsPerCell));
            }
         }
    }
}

