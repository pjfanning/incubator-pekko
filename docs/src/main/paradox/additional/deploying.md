---
project.description: How to deploy an Apache Pekko Cluster to Kubernetes and Docker.
---
# Deploying

## Deploying to Kubernetes

Deploy to Kubernetes according to the guide and example project for [Deploying a Pekko Cluster to Kubernetes]($pekko.doc.dns$/docs/pekko-management/current/kubernetes-deployment/index.html), but that requires more expertise of Kubernetes.

### Cluster bootstrap

To take advantage of running inside Kubernetes while forming a cluster, 
[Pekko Cluster Bootstrap]($pekko.doc.dns$/docs/pekko-management/current/bootstrap/) helps forming or joining a cluster using Pekko Discovery to discover peer nodes. 
with the Kubernetes API or Kubernetes via DNS.  

You can look at the
@java[[Cluster with Kubernetes example project](https://github.com/apache/incubator-pekko-samples/tree/main/pekko-sample-cluster-kubernetes-java)]
@scala[[Cluster with Kubernetes example project](https://github.com/apache/incubator-pekko-samples/tree/main/pekko-sample-cluster-kubernetes-scala)]
to see what this looks like in practice.
 
### Resource limits

To avoid CFS scheduler limits, it is best not to use `resources.limits.cpu` limits, but use `resources.requests.cpu` configuration instead.

## Deploying to Docker containers

You can use both Pekko remoting and Pekko Cluster inside Docker containers. Note
that you will need to take special care with the network configuration when using Docker,
described here: @ref:[Pekko behind NAT or in a Docker container](../remoting-artery.md#remote-configuration-nat-artery)

You can look at the
@java[[Cluster with docker-compse example project](https://github.com/apache/incubator-pekko-samples/tree/main/pekko-sample-cluster-docker-compose-java)]
@scala[[Cluster with docker-compose example project](https://github.com/apache/incubator-pekko-samples/tree/main/pekko-sample-cluster-docker-compose-scala)]
to see what this looks like in practice.

For the JVM to run well in a Docker container, there are some general (not Pekko specific) parameters that might need tuning:

### Resource constraints

Docker allows [constraining each containers' resource usage](https://docs.docker.com/config/containers/resource_constraints/).

#### Memory

You may want to look into using `-XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap` options for your JVM later than 8u131, which makes it understand c-group memory limits. On JVM 10 and later, the `-XX:+UnlockExperimentalVMOptions` option is no longer needed.

#### CPU

For multi-threaded applications such as the JVM, the CFS scheduler limits are an ill fit, because they will restrict
the allowed CPU usage even when more CPU cycles are available from the host system. This means your application may be
starved of CPU time, but your system appears idle.

For this reason, it is best to avoid `--cpus` and `--cpu-quota` entirely, and instead specify relative container weights using `--cpu-shares` instead.

