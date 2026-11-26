import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common

object ivys {
  val scala = "2.13.18"
  val chiselCrossVersions = Map(
    "xiangshan" -> (
      mvn"cc.xiangshan::chisel:7.16.0-xiangshan-2",
      mvn"cc.xiangshan:::chisel-plugin:7.16.0-xiangshan-2"
    ),
    "upstream" -> (
      mvn"org.chipsalliance::chisel:7.16.0",
      mvn"org.chipsalliance:::chisel-plugin:7.16.0"
    )
  )
  def chiselKeys = chiselCrossVersions.keys.toSeq
}

val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

trait HasChisel extends SbtModule with Cross.Module[String] {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(ivys.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy: Option[Dep] = Some(ivys.chiselCrossVersions(crossValue)._2)

  override def scalaVersion = ivys.scala

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

trait RocketChip extends `rocket-chip`.common.RocketChipModule with HasChisel {
  val rcPath = pwd / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = mvn"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = mvn"org.json4s::json4s-jackson:4.0.7"

  trait Macros extends `rocket-chip`.common.MacrosModule with HasChisel {
    def scalaVersion: T[String] = T(ivys.scala)
    def scalaReflectIvy = mvn"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  trait CDE extends `rocket-chip`.cde.common.CDEModule with Cross.Module[String] {
    def scalaVersion: T[String] = T(ivys.scala)
    override def millSourcePath = rcPath / "cde" / "cde"
  }

  trait HardFloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {
    def scalaVersion: T[String] = T(ivys.scala)
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  def macrosModule = macros(crossValue)
  def cdeModule = cde(crossValue)
  def hardfloatModule = hardfloat(crossValue)

  object macros extends Cross[Macros](ivys.chiselKeys)
  object cde extends Cross[CDE](ivys.chiselKeys)
  object hardfloat extends Cross[HardFloat](ivys.chiselKeys)
}

object rocketchip extends Cross[RocketChip](ivys.chiselKeys)

trait Utility extends HasChisel {
  override def millSourcePath = pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip(crossValue))

  override def ivyDeps = super.ivyDeps() ++ Agg(
    mvn"com.lihaoyi::sourcecode:0.4.4",
  )
}

object utility extends Cross[Utility](ivys.chiselKeys)

trait OpenNCB extends HasChisel {
  override def millSourcePath = pwd / "OpenNCB"
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip(crossValue))
}

object openNCB extends Cross[OpenNCB](ivys.chiselKeys)

trait XSCache extends HasChisel with $file.common.XSCacheModule {
  override def millSourcePath = pwd

  override def moduleDeps = super.moduleDeps ++ Seq(zhujiangCompat(crossValue))

  def rocketModule: ScalaModule = rocketchip(crossValue)

  def utilityModule: ScalaModule = utility(crossValue)

  def openNCBModule: ScalaModule = openNCB(crossValue)

  object test extends SbtTests with TestModule.ScalaTest {
    override def sources = T.sources {
      Seq(
        PathRef(pwd / "src" / "test" / "scala" / "ReplacementPolicyTest.scala"),
        PathRef(pwd / "src" / "test" / "scala" / "TestSplittedSRAM.scala")
      )
    }
  }

  object testtop extends Module {
    trait L2 extends HasChisel {
      override def millSourcePath = pwd
      override def moduleDeps = super.moduleDeps ++ Seq(XSCache(crossValue))
      override def sources = T.sources {
        Seq(PathRef(pwd / "src" / "test" / "scala" / "TestTopL2.scala"))
      }
      override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
    }

    object l2 extends Cross[L2](ivys.chiselKeys)

    trait OpenLLC extends HasChisel {
      override def millSourcePath = pwd
      override def moduleDeps = super.moduleDeps ++ Seq(XSCache(crossValue))
      override def sources = T.sources {
        Seq(PathRef(pwd / "src" / "test" / "scala" / "TestTopOpenLLC.scala"))
      }
      override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
    }

    object openllc extends Cross[OpenLLC](ivys.chiselKeys)

    trait Zhujiang extends HasChisel {
      override def millSourcePath = pwd
      override def moduleDeps = super.moduleDeps ++ Seq(XSCache(crossValue), zhujiangCompat(crossValue))
      override def sources = T.sources {
        Seq(
          PathRef(pwd / "src" / "test" / "scala" / "ZhuJiangBridge.scala"),
          PathRef(pwd / "src" / "test" / "scala" / "TestTopZhuJiang.scala")
        )
      }
      override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
    }

    object zhujiang extends Cross[Zhujiang](ivys.chiselKeys)
  }

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
}

object XSCache extends Cross[XSCache](ivys.chiselKeys)

trait ZhujiangCompat extends HasChisel {
  override def millSourcePath = pwd / "ZhuJiang"
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip(crossValue), utility(crossValue))
  override def sources = T.sources {
    Seq(
      PathRef(millSourcePath / "src" / "main" / "scala"),
      PathRef(millSourcePath / "xs-utils" / "src" / "main" / "scala")
    )
  }
  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
}

object zhujiangCompat extends Cross[ZhujiangCompat](ivys.chiselKeys)