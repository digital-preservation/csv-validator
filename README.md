CSV Validator
=============

A Validation Tool and APIs for validating CSV (Comma Separated Value) files by using [CSV Schema](https://github.com/digital-preservation/csv-schema).

[![Build Status](https://travis-ci.org/digital-preservation/csv-validator.png?branch=master)](https://travis-ci.org/digital-preservation/csv-validator)

Released under the [Mozilla Public Licence version 2.0](http://www.mozilla.org/MPL/2.0/).

A [comprehensive user guide is available in GitHub pages] (http://digital-preservation.github.io/csv-validator/), along with a more [complete specification of the CSV Schema language] (http://digital-preservation.github.io/csv-schema/csv-schema-1.0.html).


Technology
----------
The Validation tool and APIs are written in Scala 2.11 and may be used as:

* A stand-alone command line tool.

* A desktop tool, we provide a simple Swing GUI.

* A library in your Scala project.

* A library in your Java project (We provide a Java 6 interface, to make things simple for Java programmers too).

The Validation Tool and APIs can be used on any Java Virtual Machine which supports Java 6 or better. The source code is
built using the i[Apache Maven](http://apache.maven.org) build tool, by executing `mvn clean install`.

Maven Artifacts
---------------
Released Maven Artifacts can be found in Maven Central under the groupId [`uk.gov.nationalarchives`](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22uk.gov.nationalarchives%22).

Schema Examples
===============
Examples of CSV Schema can be found in the test cases of the `csv-validator-core` module. See the `*.csvs` files in [acceptance/](https://github.com/digital-preservation/csv-validator/tree/master/csv-validator-core/src/test/resources/uk/gov/nationalarchives/csv/validator/acceptance). Schemas used by the Digital Preservation department at The National Archives are also available in the `example-schemas` folder of the [csv-schema](https://github.com/digital-preservation/csv-schema) repository.


Current Limitations of the CSV Validator Tool
=============================================
The CSV Validator implements almost all of `CSV Schema 1.0` language, current limitations and missing functionality are:

* No checking of column names in the CSV Schema is performed against the CSV file; as such `@ignoreColumnNameCase` has no effect.

* `@matchIsFalse` column directive is not yet implemented (silently ignored!).

* `DateExpr` is not yet implemented (raises Schema check error).

* `PartialDateExpr` is not yet implemented (raises Schema check error).

* At least `MD5`, `SHA-1`, `SHA-2`, `SHA-3`, and `SHA-256` checksum algorithms are supported. Probably many more as well as we defer to Java's `java.security.MessageDigest` class.
