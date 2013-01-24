resolvers += "artifactory" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"

resolvers += "artifactoryi local" at "http://wb-d-tfs2.web.local:8081/artifactory/plugins-release"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.5")
