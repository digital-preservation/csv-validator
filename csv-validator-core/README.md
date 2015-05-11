CSV Validator Core
==================

Core logic for the csv validator.

###Directives

####Global directives

* totalColumns TODO

* noHeader TODO

* integrityCheck("filenameColumn","includefolder")

This directive specifies whether or not to perform an integrity check ie a check to see if all file under the content folder
are listed in the metadata file. The 1st parameter is the name of the column of the file path and the second optional parameter
is a string that take either "includeFolder" or "excludeFolder" depending on whether the different folders are listed in the metadata files.


####Column Directives

