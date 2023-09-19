FROM cgr.dev/chainguard/jre:openjdk-17

# Copy CSV Validator
COPY LICENSE /csv-validator/LICENSE
COPY lib /csv-validator/lib


# Build-time metadata as defined at http://label-schema.org
# and used by autobuilder @hooks/build
LABEL org.label-schema.build-date=${maven.build.timestamp} \
      org.label-schema.description="${project.description}" \
      org.label-schema.name="CSV Validator" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.url="${project.url}" \
      org.label-schema.vcs-ref=${build-commit-abbrev} \
      org.label-schema.vcs-url="${project.scm.url}" \
      org.label-schema.vendor="The National Archives"

ENV CLASSPATH=/csv-validator/lib/*

ENV JAVA_OPTS \
  -Dfile.encoding=UTF8 \
  -Dsun.jnu.encoding=UTF-8 \
  -Djava.awt.headless=true \
  -XX:+UseNUMA \
  -XX:+UseZGC \
  -XX:+UseStringDeduplication \
  -XX:+UseContainerSupport \
  -XX:MaxRAMPercentage=${JVM_MAX_RAM_PERCENTAGE:-75.0} \
  -XX:+ExitOnOutOfMemoryError

ENTRYPOINT [ "java", \
    "uk.gov.nationalarchives.csv.validator.cmd.CsvValidatorCmdApp"]
