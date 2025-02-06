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
        CsvValidatorJavaBridge.ValidationResult validatorRequest = (CsvValidatorJavaBridge.ValidationResult) result;
        Assert.assertEquals("csvFile", validatorRequest.validatorRequest().csvFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validatorRequest.validatorRequest().csvEncoding());
        Assert.assertEquals("csvSchema", validatorRequest.validatorRequest().csvSchemaFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validatorRequest.validatorRequest().csvSchemaEncoding());
        Assert.assertFalse(validatorRequest.validatorRequest().failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validatorRequest.validatorRequest().pathSubstitutionsList());
        Assert.assertFalse(validatorRequest.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertFalse(validatorRequest.validatorRequest().trace());
        Assert.assertNull(validatorRequest.validatorRequest().progress());
        Assert.assertFalse(validatorRequest.validatorRequest().skipFileChecks());
        Assert.assertEquals(4096, validatorRequest.validatorRequest().maxCharsPerCellLimit());
    }

    @Test
    public void defaultValuesAreUsedWhenUsingReaderMethod() {
        Result result = new CsvValidator.ValidatorBuilder(csvFileReader, csvSchemaReader).runValidation();
        CsvValidatorJavaBridge.ReaderValidationResult validatorRequest = (CsvValidatorJavaBridge.ReaderValidationResult) result;
        Assert.assertEquals(csvFileReader, validatorRequest.validatorRequest().csvReader());
        Assert.assertEquals(csvSchemaReader, validatorRequest.validatorRequest().csvSchemaReader());
        Assert.assertFalse(validatorRequest.validatorRequest().failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validatorRequest.validatorRequest().pathSubstitutionsList());
        Assert.assertFalse(validatorRequest.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertFalse(validatorRequest.validatorRequest().trace());
        Assert.assertNull(validatorRequest.validatorRequest().progress());
        Assert.assertFalse(validatorRequest.validatorRequest().skipFileChecks());
        Assert.assertEquals(4096, validatorRequest.validatorRequest().maxCharsPerCellLimit());
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
        CsvValidatorJavaBridge.ValidationResult validatorRequest = (CsvValidatorJavaBridge.ValidationResult) result;
        Assert.assertEquals("csvFile", validatorRequest.validatorRequest().csvFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validatorRequest.validatorRequest().csvEncoding());
        Assert.assertEquals("csvSchema", validatorRequest.validatorRequest().csvSchemaFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validatorRequest.validatorRequest().csvSchemaEncoding());
        Assert.assertTrue(validatorRequest.validatorRequest().failFast());
        Assert.assertEquals("fromPath", validatorRequest.validatorRequest().pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validatorRequest.validatorRequest().pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validatorRequest.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertTrue(validatorRequest.validatorRequest().trace());
        Assert.assertEquals(progressCallback, validatorRequest.validatorRequest().progress());
        Assert.assertTrue(validatorRequest.validatorRequest().skipFileChecks());
        Assert.assertEquals(8096, validatorRequest.validatorRequest().maxCharsPerCellLimit());
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

        CsvValidatorJavaBridge.ReaderValidationResult validatorRequest = (CsvValidatorJavaBridge.ReaderValidationResult) result;
        Assert.assertEquals(csvFileReader, validatorRequest.validatorRequest().csvReader());
        Assert.assertEquals(csvSchemaReader, validatorRequest.validatorRequest().csvSchemaReader());
        Assert.assertTrue(validatorRequest.validatorRequest().failFast());
        Assert.assertEquals("fromPath", validatorRequest.validatorRequest().pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validatorRequest.validatorRequest().pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validatorRequest.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertTrue(validatorRequest.validatorRequest().trace());
        Assert.assertEquals(progressCallback, validatorRequest.validatorRequest().progress());
        Assert.assertTrue(validatorRequest.validatorRequest().skipFileChecks());
        Assert.assertEquals(16096, validatorRequest.validatorRequest().maxCharsPerCellLimit());
    }
}

