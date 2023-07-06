/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, which was derived from Akka.
 */

import sbt.Keys._
import sbt._

/**
 * Copies LICENSE and NOTICE files into jar META-INF dir
 */
object AddMetaInfLicenseFiles extends AutoPlugin {

  private lazy val baseDir = LocalRootProject / baseDirectory

  lazy val apacheSonatypeLicenseFile: SettingKey[File] =
    settingKey[File]("The LICENSE file which needs to be included in published artifact")
  lazy val apacheSonatypeNoticeFile: SettingKey[File] =
    settingKey[File]("The NOTICE file which needs to be included in published artifact")
  lazy val apacheSonatypeDisclaimerFile: SettingKey[Option[File]] =
    settingKey[Option[File]]("The NOTICE file which needs to be included in published artifact")

  lazy val sbtMavenProjectSettings: Seq[Setting[_]] = Seq(
    licenses ++= {
      val log = sLog.value
      val currentLicenses = licenses.value
      currentLicenses.collectFirst { case (field, url) if field.contains("Apache") => url } match {
        case Some(url) =>
          log.warn(
            s"No Apache license added in project ${projectID.value} since a duplicate license has already been detected with url: ${url.toString}, please remove it")
          Nil
        case None => Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"))
      }
    },
    apacheSonatypeLicenseFile := baseDir.value / "LICENSE",
    apacheSonatypeNoticeFile := baseDir.value / "NOTICE",
    apacheSonatypeDisclaimerFile := None) ++ inConfig(Compile)(
    Seq(
      resourceGenerators += {
        Def.task {
          val dir = resourceManaged.value
          List(
            addFileToMetaInf(dir, apacheSonatypeLicenseFile.value, Some("LICENSE")),
            addFileToMetaInf(dir, apacheSonatypeNoticeFile.value, Some("NOTICE"))) ++ apacheSonatypeDisclaimerFile.value
            .map(disclaimerFile => addFileToMetaInf(dir, disclaimerFile))
            .toList
        }
      },
      // See https://issues.apache.org/jira/browse/LEGAL-28
      packageSrc / mappings ++= {
        Seq(
          apacheSonatypeLicenseFile.value -> "META-INF/LICENSE",
          apacheSonatypeNoticeFile.value -> "META-INF/NOTICE") ++ apacheSonatypeDisclaimerFile.value.map(path =>
          path -> "META-INF/DISCLAIMER").toSeq
      },
      packageDoc / mappings ++= {
        Seq(
          apacheSonatypeLicenseFile.value -> "META-INF/LICENSE",
          apacheSonatypeNoticeFile.value -> "META-INF/NOTICE") ++ apacheSonatypeDisclaimerFile.value.map(path =>
          path -> "META-INF/DISCLAIMER").toSeq
      }))

  override lazy val projectSettings = sbtMavenProjectSettings ++ Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "StandardLicense.txt",
    apacheSonatypeNoticeFile := baseDir.value / "legal" / "PekkoNotice.txt",
    apacheSonatypeDisclaimerFile := Some(baseDir.value / "DISCLAIMER"))

  /**
   * Settings specific for Pekko actor subproject which requires a different license file.
   */
  lazy val actorSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-actor-jar-license.txt",
    apacheSonatypeNoticeFile := baseDir.value / "legal" / "pekko-actor-jar-notice.txt")

  /**
   * Settings specific for Pekko actor subproject which requires a different license file.
   */
  lazy val clusterSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-cluster-jar-license.txt")

  /**
   * Settings specific for Pekko distributed-data subproject which requires a different license file.
   */
  lazy val distributedDataSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-distributed-data-jar-license.txt")

  /**
   * Settings specific for Pekko persistence-typed subproject which requires a different license file.
   */
  lazy val persistenceTypedSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-persistence-typed-jar-license.txt")

  /**
   * Settings specific for Pekko remote subproject which requires a different license file.
   */
  lazy val remoteSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-remote-jar-license.txt",
    apacheSonatypeNoticeFile := baseDir.value / "legal" / "pekko-remote-jar-notice.txt")

  /**
   * Settings specific for Pekko protobuf subproject which requires a different license file
   * as well as an additional "COPYING.protobuf" file.
   */
  lazy val protobufSettings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-protobuf-jar-license.txt") ++ inConfig(Compile)(Seq(
    resourceGenerators += {
      Def.task {
        List(
          addFileToMetaInf(resourceManaged.value, baseDir.value / "COPYING.protobuf"))
      }
    }))

  /**
   * Settings specific for Pekko protobuf-v3 subproject which requires a different license file
   * as well as an additional "COPYING.protobuf" file.
   */
  lazy val protobufV3Settings = Seq(
    apacheSonatypeLicenseFile := baseDir.value / "legal" / "pekko-protobuf-v3-jar-license.txt") ++ inConfig(Compile)(
    Seq(
      resourceGenerators += {
        Def.task {
          List(
            addFileToMetaInf(resourceManaged.value, baseDir.value / "COPYING.protobuf"))
        }
      }))

  /**
   * Adds a file to the a specified resourceManaged `META-INF` folder so that it will get included by sbt when
   * generating artifacts
   *
   * @see
   * https://www.scala-sbt.org/1.x/docs/Howto-Generating-Files.html#Generate+resources
   * @param resourceManagedDir
   * sbt's resource managed directory typically derived by using `resourcedManaged.value`, the file will get copied
   * in this location under the `META-INF` folder.
   * @param file
   * The file you want to add to the META-INF folder
   * @param targetFileName
   * The target file name, if not specified then it uses the same filename specified in `file`.
   * @return
   * The resulting [[File]] which was added
   */
  final def addFileToMetaInf(resourceManagedDir: File, file: File, targetFileName: Option[String] = None): File = {
    val toFile = resourceManagedDir / "META-INF" / targetFileName.getOrElse(file.getName)
    IO.copyFile(file, toFile)
    toFile
  }

  override def trigger = allRequirements
}
