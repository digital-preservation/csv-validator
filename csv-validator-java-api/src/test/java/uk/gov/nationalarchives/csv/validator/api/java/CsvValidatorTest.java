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
        CsvValidatorJavaBridge.ValidationResult validationResult = (CsvValidatorJavaBridge.ValidationResult) result;
        Assert.assertEquals("csvFile", validationResult.validatorRequest().csvFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validationResult.validatorRequest().csvEncoding());
        Assert.assertEquals("csvSchema", validationResult.validatorRequest().csvSchemaFile());
        Assert.assertEquals(StandardCharsets.UTF_8, validationResult.validatorRequest().csvSchemaEncoding());
        Assert.assertFalse(validationResult.validatorRequest().failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validationResult.validatorRequest().pathSubstitutionsList());
        Assert.assertFalse(validationResult.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertFalse(validationResult.validatorRequest().trace());
        Assert.assertNull(validationResult.validatorRequest().progress());
        Assert.assertFalse(validationResult.validatorRequest().skipFileChecks());
        Assert.assertEquals(4096, validationResult.validatorRequest().maxCharsPerCellLimit());
    }

    @Test
    public void defaultValuesAreUsedWhenUsingReaderMethod() {
        Result result = new CsvValidator.ValidatorBuilder(csvFileReader, csvSchemaReader).runValidation();
        CsvValidatorJavaBridge.ReaderValidationResult validationResult = (CsvValidatorJavaBridge.ReaderValidationResult) result;
        Assert.assertEquals(csvFileReader, validationResult.validatorRequest().csvReader());
        Assert.assertEquals(csvSchemaReader, validationResult.validatorRequest().csvSchemaReader());
        Assert.assertFalse(validationResult.validatorRequest().failFast());
        Assert.assertEquals(new ArrayList<Substitution>(), validationResult.validatorRequest().pathSubstitutionsList());
        Assert.assertFalse(validationResult.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertFalse(validationResult.validatorRequest().trace());
        Assert.assertNull(validationResult.validatorRequest().progress());
        Assert.assertFalse(validationResult.validatorRequest().skipFileChecks());
        Assert.assertEquals(4096, validationResult.validatorRequest().maxCharsPerCellLimit());
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
        CsvValidatorJavaBridge.ValidationResult validationResult = (CsvValidatorJavaBridge.ValidationResult) result;
        Assert.assertEquals("csvFile", validationResult.validatorRequest().csvFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validationResult.validatorRequest().csvEncoding());
        Assert.assertEquals("csvSchema", validationResult.validatorRequest().csvSchemaFile());
        Assert.assertEquals(StandardCharsets.ISO_8859_1, validationResult.validatorRequest().csvSchemaEncoding());
        Assert.assertTrue(validationResult.validatorRequest().failFast());
        Assert.assertEquals("fromPath", validationResult.validatorRequest().pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validationResult.validatorRequest().pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validationResult.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertTrue(validationResult.validatorRequest().trace());
        Assert.assertEquals(progressCallback, validationResult.validatorRequest().progress());
        Assert.assertTrue(validationResult.validatorRequest().skipFileChecks());
        Assert.assertEquals(8096, validationResult.validatorRequest().maxCharsPerCellLimit());
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

        CsvValidatorJavaBridge.ReaderValidationResult validationResult = (CsvValidatorJavaBridge.ReaderValidationResult) result;
        Assert.assertEquals(csvFileReader, validationResult.validatorRequest().csvReader());
        Assert.assertEquals(csvSchemaReader, validationResult.validatorRequest().csvSchemaReader());
        Assert.assertTrue(validationResult.validatorRequest().failFast());
        Assert.assertEquals("fromPath", validationResult.validatorRequest().pathSubstitutionsList().get(0).getFrom());
        Assert.assertEquals("toPath", validationResult.validatorRequest().pathSubstitutionsList().get(0).getTo());
        Assert.assertTrue(validationResult.validatorRequest().enforceCaseSensitivePathChecks());
        Assert.assertTrue(validationResult.validatorRequest().trace());
        Assert.assertEquals(progressCallback, validationResult.validatorRequest().progress());
        Assert.assertTrue(validationResult.validatorRequest().skipFileChecks());
        Assert.assertEquals(16096, validationResult.validatorRequest().maxCharsPerCellLimit());
    }
}

