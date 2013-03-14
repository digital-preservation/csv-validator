package uk.gov.tna.dri.schema;

import uk.gov.tna.dri.validator.MetaDataValidatorCommandLineApp;

import java.util.ArrayList;
import java.util.List;

/**
 * Validate that a cvs file matches a format specified by a schema.
 * This is a Java wrapper calling the main Scala application.
 */
public class Validator {

    /**
     * Check that a cvs formatted file matches a schema file.
     *
     * @param cvsFilename Filename of cvs file
     * @param schemaFilename Filename of schema file
     * @param failFast  True if want to stop on first cvs file error
     * @param pathSubstitutions list of substitutions for file paths
     * @return empty list of no errors or list of error strings.
     */
    public static List<String> validate( String cvsFilename, String schemaFilename, Boolean failFast, List<Substitution> pathSubstitutions) {
        return MetaDataValidatorCommandLineApp.javaProcessMetaData(cvsFilename, schemaFilename, failFast, pathSubstitutions);
    }

    /**
     * A path substitution line for file access where cvs given paths need to be replaced.
     */
    public static class Substitution {
        private String from;
        private String to;

        public Substitution( String from, String to) {
            this.from = from;
            this.to = to;
        }

        public String getFrom() { return from; }

        public String getTo() { return to; }
    }


    /**
     * DEMO on how to use the java interface to call the validator
     *
     * @param args not used
     */
    public static void main(String[] args) {

        Boolean failFast = false;
        List<Substitution> pathSubstitutions = new ArrayList<Substitution>();

        // add a substitution path
//        pathSubstitutions.add( new Substitution( "file://something", "/home/xxx") );  // add a substitution path

        List<String> messages = uk.gov.tna.dri.schema.Validator.validate(
                "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/metaData.csv",
                "/home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/schema.txt",
                failFast,
                pathSubstitutions
        );

        if(messages.isEmpty()) {
            System.out.println( "All worked OK");
        } else {
            for (String message : messages) {
                System.out.println("ERROR: " + message);
            }
        }
    }
}

