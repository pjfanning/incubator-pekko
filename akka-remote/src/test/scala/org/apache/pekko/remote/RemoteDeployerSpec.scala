/*
 * Copyright (C) 2009-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package org.apache.pekko.remote

import com.typesafe.config._

import org.apache.pekko
import pekko.ConfigurationException
import pekko.actor._
import pekko.routing._
import pekko.testkit._

object RemoteDeployerSpec {
  val deployerConf = ConfigFactory.parseString(
    """
      pekko.actor.provider = remote
      pekko.actor.deployment {
        /service2 {
          router = round-robin-pool
          nr-of-instances = 3
          remote = "akka://sys@wallace:2552"
          dispatcher = mydispatcher
        }
      }
      pekko.remote.classic.netty.tcp.port = 0
      """,
    ConfigParseOptions.defaults)

  class RecipeActor extends Actor {
    def receive = { case _ => }
  }

}

class RemoteDeployerSpec extends PekkoSpec(RemoteDeployerSpec.deployerConf) {

  "A RemoteDeployer" must {

    "be able to parse 'pekko.actor.deployment._' with specified remote nodes" in {
      val service = "/service2"
      val deployment = system.asInstanceOf[ActorSystemImpl].provider.deployer.lookup(service.split("/").drop(1))

      deployment should ===(
        Some(
          Deploy(
            service,
            deployment.get.config,
            RoundRobinPool(3),
            RemoteScope(Address("akka", "sys", "wallace", 2552)),
            "mydispatcher")))
    }

    "reject remote deployment when the source requires LocalScope" in {
      intercept[ConfigurationException] {
        system.actorOf(Props.empty.withDeploy(Deploy.local), "service2")
      }.getMessage should ===(
        "configuration requested remote deployment for local-only Props at [akka://RemoteDeployerSpec/user/service2]")
    }

  }

}
