CSV Validator
=============

A Schema Language and Validation Tool for CSV (Comma Separated Value) files.

[![Build Status](https://travis-ci.org/digital-preservation/csv-validator.png?branch=master)](https://travis-ci.org/digital-preservation/csv-validator)

Released under the [Mozilla Public Licence version 2.0](http://www.mozilla.org/MPL/2.0/).

A brief introduction follows before, but a [more comprehensive user guide is available in GitHub pages] (https://digital-preservation.github.io/csv-validator/), along with a more [complete sepcification the CSV Schema language] (https://digital-preservation.github.io/csv-validator/csv-schema-1.0.html).

Approach
--------
Firstly, we have defined a Grammar which describes a language for expressing rules to validate a CSV file. We call such an expression of this language a CSVS (CSV Schema). The grammar itself is described in [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) and is available in the [csvschema-grammar.ebnf](blob/master/csv-schema-grammar.ebnf) file.

Secondly, we built an implementation that will take a CSV Schema and a CSV file and assert that each rule in the CSV 
Schema holds true for the CSV file. The validaton tool is written in Scala 2.10 and may be used as:

* A stand-alone command line tool.

* A desktop tool, we provide a simple Swing GUI.

* A library in your Scala project.

* A library in your Java project (We provide a Java 6 interface, to make things simple for Java programmers too).

Use Case
--------
TNA (The National Archives) receive Metadata along with Collections of Digitised or Born-Digital Collections. Whilst TNA typically process Metadata in XML and RDF, it was recognised that it was too difficult and/or expensive for many suppliers to produce the desired metadata in XML and/or RDF, as such it was decided that Metadata would be received in CSV format.

Our experience shows that when suppliers are asked to produce metadata in XML or RDF there are several possible barriers:

* Many content/document repository systems only export metadata in CSV, or generate XML or RDF in a non-desirable format which would then have to be transformed (at further cost).

* Lack of technical knowledge in either XML or RDF.

* Lack of experience of tools for producing and validating XML or RDF.

* Cost. Installing new software tools comes at a severe cost for those OGDs that have outsourced their IT support.
 
* Best/Worst case, most suppliers already have Microsoft Excel (or an equivalent) installed which they know how to use to produce a CSV file.


TNA set exacting requirements on the Metadata that they expect and the format of that Metadata. Such constraints enable them to automatically process it, as the semantics of the metadata are already defined. Whilst previous bespoke tools have been developed in the past for validating data in various CSV files, it was felt that a generic *open* tool which could be shared with suppliers would offer several benefits:

* A common CSV Schema language, would enable TNA to absolutely define required Metadata formats.

* Developed CSV Schemas could be shared with suppliers and other archival sector organisations.

* Suppliers could validate Metadata before sending it to TNA. Hopefully reducing mistakes and therefore costs to both parties.

* TNA could use the same tool to ensure Metadata compliance automatically.
 
* Although not of primary concern, it was recognised that this tool would also have value for anyone working with CSV as a data/metadata transfer medium.

Philosophy
----------
A few bullet-points that will try and help to explain our thinking in the design of both the CSV Schema Language and the Validation Tool implementation:

* Simple CSV Schema Language.
A DSL (Domain Specifc Language) was desired that could be expressed in plain text and should be simple enough that Metadata experts could easily write it without having to know a programming language or data/document modelling language such as XML or RDF. Note, the CSV Schema Language is **NOT** itself expressed in CSV, it is expressed in a simple text format.

* Context is King!
Schema rules are written for each column of the CSV file. Each set of column rules is then asserted against each row of the CSV file in turn. Each rule in the CSV Schema operates on the current context (e.g. defined Column and parsed Row), unless otherwise specified. Hopefully this makes the rules short and concise.

* Streaming.
Often the Metadata files that we receive are very large as they contain many records about a Collection which itself can be huge. The CSV Schema Language was designed with an eye to being able to write a Validation tool which could read the CSV file as a stream. Few steps require mnenomization of data from the CSV file, and where they do this is limited and should be easily optimisable to keep memory use to a minimum.

* Sane Defaults.
We try to do the right thing by default, CSV files and their bretheren (Tab Separated Values etc.) can come in many shapes and sizes, by default we parse CSV according to [RFC 4180](http://tools.ietf.org/html/rfc4180 "Common Format and MIME Type for Comma-Separated Values (CSV) Files"), of course we allow you to customize this behaviour in the CSV Schema.

* CSV Schema is ***NOT*** a Programming Language.
This is worth stressing as it was something we had to keep site of ourselves during development; CSV Schema is a simple data definition and validation language for CSV!


Schema Reference
================
Examples of CSV Schema can be found in the test cases of the csv-core module. See the *.csvs files in [acceptance/](tree/master/csv-validator-core/src/test/resources/uk/gov/nationalarchives/csv/validator/acceptance/).


Global Directives
-----------------
Global Directives influence the processing of the CSV file and are applied to the further rules in the CSV Schema.

- - -
```csvs
@totalColumns n
```
Asserts that the CSV file MUST have `n` columns on each line. Will also be checked against the number of `column validation rules` in the CSV Schema.
- - -
```csvs
@noHeader
```
Asserts that the CSV file SHALL NOT have a header line which contains the column names. All `column validation rules` will be asserted from line 1 of the CSV file, rather than line 2.

Comments
--------
Comments in CSV Schema are the same as those in C++ and Java.

```csvs
// this is a single line comment

// this is another single line comment
```

```csvs
/* this is a multi-line
comment */

/*
	This is another multi-line
	comment
*/
```

General Column Validation Rules
-------------------------------
```csvs
notEmpty
```
Asserts that the value of the current row at the declared column is not-empty. That is to say there must be some value present whatever it is.
- - -
```csvs
length(n)
```
Asserts that the value of the current row at the declared column consists of `n` characters.

```csvs
length(n, *)
```
Asserts that the value of the current row at the declared column consists of at least `n` characters.

```csvs
length(*, n)
```
Asserts that the value of the current row at the declared column consists of at most `n` characters.

```csvs
length(n1, n2)
```
Asserts that the value of the current row at the declared column consists of at least `n1` and no more than `n2` characters.

String Column Validation Rules
------------------------------
```csvs
is(string-provider)
```
Asserts that the value of the current row at the declared column is that of the `string-provider`. The `string-provider` may be either a String Literal e.g. "some-value" or a reference to another column in the same row e.g. $some-other-column.
- - - 
```csvs
not(string-provider)
```
Asserts the inverse of `is`. That is to say that, it asserts that the value of the current row at the declared column is NOT that of the `string-provider`.
- - -
```csvs
in(string-provider)
```
Asserts that the value of the current row at the declared column is in the value of the `string-provider`.
- - -
```csvs
starts(string-provider)
```
Asserts that the value of the current row at the declared column starts with the value of the `string-provider`.
- - -
```csvs
ends(string-provider)
```
Compliment of `starts`. That is to say that, the value of the current row at the declared column ends with the value of the `string-provider`.
- - -
```csvs
regex(string-provider)
```
Asserts that the value of the current row at the declared column matches the Regular Expression provided by the value of the `string-provider`.


Numeric Column Validation Rules
-------------------------------
- - -
```csvs
range(n1, n2)
```
Asserts that the value of the current row at the declared column lies between the numbers `n1` and `n2` inclusive.
