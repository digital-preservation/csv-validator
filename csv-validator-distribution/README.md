CSV Validator Application
=========================

This module assembles the csv-validator deliverable in a zip format. 

---
### Structure of the generated ZIP file 
The zip, which includes bundled JRE, has the following structure:

```
  csv-validator-application-<version_number>
       |
       +--- csv-validator-gui.bat
       |
       +--- csv-validator-cmd.bat
       |
       +--- csv-validator-cmd-<Version Number>.jar
       |
       +--- csv-validator-ui-<Version Number>.jar
       |
       +--- LICENSE
       |
       +-- [lib]
       |     |
       |     +--- <various application jars and dependencies>
       |    
       +-- [jre]
             |
             +--- <bundled jre> 

```

The zip file without JRE has a similar structure as above, except the `[JRE]` folder and the scripts are made as 
shell scripts rather than Windows batch files 

```
  csv-validator-application-<version_number>
       |
       +--- csv-validator-gui
       |
       +--- csv-validator-cmd
       |
       +--- csv-validator-cmd-<Version Number>.jar
       |
       +--- csv-validator-ui-<Version Number>.jar
       |
       +--- LICENSE
       |
       +-- [lib]
             |
             +--- <various application jars and dependencies>

```
