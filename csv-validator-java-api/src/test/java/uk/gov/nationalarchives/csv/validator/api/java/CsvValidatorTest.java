/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java;

import org.junit.Assert;
import org.junit.Test;

import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import static org.junit.Assert.assertThrows;

public class CsvValidatorTest {
    private final StringReader csvFileReader = new StringReader("csvFile");
    private final StringReader csvSchemaReader = new StringReader("csvSchemaReader");

    @Test
    public void defaultValuesAreUsedWhenUsingTextFileMethod() {
        Result result = new CsvValidator.ValidatorBuilder("csvFile", "csvSchema").runValidation();
        CsvValidatorJavaBridge.ValidationRequest validationRequest = ((CsvValidatorJavaBridge.ValidationResult) result).validatorRequest();
        Assert.assertEquals("csvFile", validationRequest.csvFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validationRequest.csvEncoding());
        Assert.assertEquals("csvSchema", validationRequest.csvSchemaFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validationRequest.csvSchemaEncoding());
        Assert.assertFalse(validationRequest.failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validationRequest.pathSubstitutionsList());
        Assert.assertFalse(validationRequest.enforceCaseSensitivePathChecks());
        Assert.assertFalse(validationRequest.trace());
        Assert.assertNull(validationRequest.progress());
        Assert.assertFalse(validationRequest.skipFileChecks());
        Assert.assertEquals(4096, validationRequest.maxCharsPerCellLimit());
    }

    @Test
    public void defaultValuesAreUsedWhenUsingReaderMethod() {
        Result result = new CsvValidator.ValidatorBuilder(csvFileReader, csvSchemaReader).runValidation();
        CsvValidatorJavaBridge.ReaderValidationRequest validationRequest = ((CsvValidatorJavaBridge.ReaderValidationResult) result).validatorRequest();
        Assert.assertEquals(csvFileReader, validationRequest.csvReader());
        Assert.assertEquals(csvSchemaReader, validationRequest.csvSchemaReader());
        Assert.assertFalse(validationRequest.failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validationRequest.pathSubstitutionsList());
        Assert.assertFalse(validationRequest.enforceCaseSensitivePathChecks());
        Assert.assertFalse(validationRequest.trace());
        Assert.assertNull(validationRequest.progress());
        Assert.assertFalse(validationRequest.skipFileChecks());
        Assert.assertEquals(4096, validationRequest.maxCharsPerCellLimit());
    }

    @Test
    public void exceptionIsThrownIfValidatingUtf8IsTrueForNonUtf8Encoding() {
        assertThrows(
                Exception.class, () ->
                new CsvValidator.ValidatorBuilder("csvFile", "csvSchema")
                    .usingCsvEncoding(StandardCharsets.ISO_8859_1, true)
                    .runValidation()
        );
    }

    @Test
    public void correctValuesArePassedIntoValidateMethodUsingTextFileMethod() throws Exception {
        ArrayList<Substitution> substitution = new ArrayList<>();
        substitution.add(new Substitution("fromPath", "toPath"));
        ProgressCallback progressCallback = complete -> {};
        Result result = new CsvValidator.ValidatorBuilder("csvFile", "csvSchema")
                .usingCsvEncoding(StandardCharsets.ISO_8859_1, false)
                .usingCsvSchemaEncoding(StandardCharsets.ISO_8859_1)
                .usingFailFast(true)
                .usingPathSubstitutions(substitution)
                .usingEnforceCaseSensitivePathChecks(true)
                .usingTrace(true)
                .usingProgress(progressCallback)
                .usingSkipFileChecks(true)
                .usingMaxCharsPerCell(8096)
                .runValidation();
        CsvValidatorJavaBridge.ValidationRequest validationRequest = ((CsvValidatorJavaBridge.ValidationResult) result).validatorRequest();
        Assert.assertEquals("csvFile", validationRequest.csvFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validationRequest.csvEncoding());
        Assert.assertEquals("csvSchema", validationRequest.csvSchemaFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validationRequest.csvSchemaEncoding());
        Assert.assertTrue(validationRequest.failFast());
        Assert.assertEquals("fromPath", validationRequest.pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validationRequest.pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validationRequest.enforceCaseSensitivePathChecks());
        Assert.assertTrue(validationRequest.trace());
        Assert.assertEquals(progressCallback, validationRequest.progress());
        Assert.assertTrue(validationRequest.skipFileChecks());
        Assert.assertEquals(8096, validationRequest.maxCharsPerCellLimit());
    }

    @Test
    public void correctValuesArePassedIntoValidateMethodUsingReaderMethod() {
        ArrayList<Substitution> substitution = new ArrayList<>();
        substitution.add(new Substitution("fromPath", "toPath"));
        ProgressCallback progressCallback = complete -> {};
        Result result = new CsvValidator.ValidatorBuilder(csvFileReader, csvSchemaReader)
                .usingFailFast(true)
                .usingPathSubstitutions(substitution)
                .usingEnforceCaseSensitivePathChecks(true)
                .usingTrace(true)
                .usingProgress(progressCallback)
                .usingSkipFileChecks(true)
                .usingMaxCharsPerCell(16096)
                .runValidation();

        CsvValidatorJavaBridge.ReaderValidationRequest validationRequest = ((CsvValidatorJavaBridge.ReaderValidationResult) result).validatorRequest();
        Assert.assertEquals(csvFileReader, validationRequest.csvReader());
        Assert.assertEquals(csvSchemaReader, validationRequest.csvSchemaReader());
        Assert.assertTrue(validationRequest.failFast());
        Assert.assertEquals("fromPath", validationRequest.pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validationRequest.pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validationRequest.enforceCaseSensitivePathChecks());
        Assert.assertTrue(validationRequest.trace());
        Assert.assertEquals(progressCallback, validationRequest.progress());
        Assert.assertTrue(validationRequest.skipFileChecks());
        Assert.assertEquals(16096, validationRequest.maxCharsPerCellLimit());
    }
}

