CSV Validator Application
=========================

This module assembles the csv-validator deliverable in a zip format. 

---
### Structure of the generated ZIP file 
The zip, which includes bundled JRE, has following structure.  

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
---
### Setting the maximum heap memory for the application
Both, the Windows batch files and the shell scripts have the ability to configure maximum memory used by the JVM
which runs CSV Validator. If you find that you are getting an error related to not having enough heap memory, you can 
increase the memory by following the steps given below:

On Windows, assuming you want to run the application with 2048MB (2GB) of heap memory 
* Open a command prompt
* type `set csvValidatorMemory=2048`
* Without closing the command prompt, within the same window, launch the batch file 

On Linux, assuming you want to run the application with 2048MB (2GB) of heap memory 
* Open a terminal window
* type `export csvValidatorMemory=2048`
* Without closing the terminal window, within the same window, call the shell script

---
### Launching the application as GUI or CLI 

On Windows,    
* To launch the GUI, 
  * open command prompt, 
  * navigate to the folder where the zip is expanded 
  * type `csv-validator-gui.bat`

* To launch the CLI,
    * open command prompt,
    * navigate to the folder where the zip is expanded
    * type `csv-validator-cmd.bat <path to csv file> <path to csv schema file>`
    * you can type `csv-validator-cmd.bat --help` to see other CLI options

On Linux,
* To launch the GUI,
    * open terminal window,
    * navigate to the folder where the zip is expanded
    * type `./csv-validator-gui`

* To launch the CLI,
    * open terminal window,
    * navigate to the folder where the zip is expanded
    * type `./csv-validator-cmd <path to csv file> <path to csv schema file>`
    * you can type `./csv-validator-cmd --help` to see other CLI options