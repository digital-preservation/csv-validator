CSV Validator - Command Line
============================

A simple command line utility for running the CSV Validator.

Designed and tested on both Windows and Linux/Unix/Mac platforms.


Basic Usage
-----------

The following command will show the useage for the application:

```bash
$ validate
```

which results in:

```bash
$ validate
CSV Validator - Command Line
Usage: validate [options] <csv-path> <csv-schema-path>

  -f <value> | --fail-fast <value>
        Stops on the first validation error rather than reporting all errors
  -p:<key>=<value> | --path:<key>=<value>
        Allows you to substitute a file path (or part of) in the CSV for a different file path
  -c <value> | --case-sensitive-paths <value>
        Enforces case-sensitive file path checking. Useful when validating on case-insensitive filesystems like Windows NTFS
  -x <value> | --csv-encoding <value>
        Defines the charset encoding used in the CSV file
  -y <value> | --csv-schema-encoding <value>
        Defines the charset encoding used in the CSV Schema file
  -i <value> | --integrity-check <value>
        Defines the filename column in the schema for integrity check
  -d <value> | --include-directory <value>
        Specifies whether or not the metadata list the directories for integrity check
  <csv-path>
        The path to the CSV file to validate
  <csv-schema-path>  
        The path to the CSV Schema file to use for validation


```


Windows Users
-------------

Instead of using `validate` simply use `validate.bat`


Building from Source Code
-------------------------

We are using the Maven build system, executing `mvn package` will produce a distribution of the command line application in both `target/csv-validator-cmd-?-application` and `target/csv-validator-cmd-?-application.zip`. Note the '?' will be replaced by the version number of the product.