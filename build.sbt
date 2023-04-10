lazy val root = project
  .in(file("."))
  .aggregate(elodin.js, elodin.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val elodin = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }
  .settings(
    name := "elodin",
    version := "0.1-SNAPSHOT",
    scalaVersion := "3.2.1",
    scalacOptions ++= Seq(
      "-feature",
      "-language:implicitConversions",
      "-Yretain-trees"
    )
  )
  .jvmSettings(
    run / fork := true,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )
  .jsSettings(
    // JS Deps
    Compile / npmDependencies ++= Seq(
      "three" -> "0.131.0",
      "monaco-editor" -> "0.30.1"
    ),

    // Scala Deps
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-dom" % "1.2.0").cross(CrossVersion.for3Use2_13),
      ("org.scalameta" %%% "scalameta" % "4.4.28").cross(CrossVersion.for3Use2_13),
      "io.github.dcascaval" %%% "scala-threejs-facades" % "0.131.0"
    ),
    scalaJSUseMainModuleInitializer := true,

    // webpackEmitSourceMaps := false,
    webpackBundlingMode := BundlingMode.LibraryOnly(),
    // webpackConfigFile := Some(baseDirectory.value / "../elodin.webpack.config.js"),

    // Add support for the DOM in `run` and `test`
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
