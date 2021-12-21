# Set up a worker for evaluation of InterventionEvaluatR
setupWorker = function() {
  if(getOption("ie.worker.local", TRUE)) {
    setupLocalWorker()
  } else {
    setupRemoteWorker()
  }
}

setupLocalWorker = function() {
  list(local=TRUE, startCluster=NULL, stopCluster=NULL)
}

setupRemoteWorker = function() {
  machineName = sprintf("iew-%s", UUIDgenerate())

  tryCatch({
    # Provision a DigitalOcean droplet
    analysisStatusDetail("Provisioning DO droplet")
    check.call(
      c(
        getOption("ie.webui.docker-machine", "docker-machine"), "create",
        "--driver", "digitalocean",
        "--digitalocean-access-token", getOption("ie.digitalocean.access.token"),
        "--digitalocean-size", getOption("ie.worker.digitalocean-droplet-size", "s-4vcpu-8gb"),
        "--digitalocean-userdata", "worker/cloud-config.yml",
        machineName
      )
    )
  
    workerIp = check.output(
      c(
        getOption("ie.webui.docker-machine", "docker-machine"), "ip",
        machineName
      )
    ) %>% trimws()
  
    makeCluster = function(workerCount) {
      makeClusterPSOCK(
          workers=rep(workerIp, workerCount),
          rshopts=c("-i", "worker/id_rsa", "-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null"),
          user="evaluatr",
          rscript="/usr/local/bin/Rscript-docker",
          connectTimeout=5*60, # Increase connection timeout to 5min to give the worker more time to pull the image
          verbose=TRUE,
          port=floor(runif(1, min=8800, max=8900))
        )
    }
  
    # First make a single-worker cluster, which we will use to determine the number of cores available on the worker
    # Repeat this a few times just to guard against transient worker startup errors
    # (Which can potentially occur, for example, due to a transient delay in docker pull at worker startup)
    workerCores = function() {
      singleCluster = makeCluster(1)
      numCores = clusterCall(singleCluster, function() { future::availableCores(methods=c("system")) })[[1]]
      stopCluster(singleCluster)
      return(numCores)
    }

    for(i in 1:3) {
      tryCatch({
        message(sprintf("workerCores attempt #%d", i))
        numCores = workerCores()
      }, error = function(errorCondition) {
        message("failed")
      })
    }
    message(sprintf("workerCores last attempt", i))
    numCores = workerCores()
    message(sprintf("succeeded"))
    
    list(
      local=FALSE, 
      startCluster=function() { makeCluster(numCores) }, 
      stopCluster=function(cluster) { stopCluster(cluster) }, 
      machineName=machineName
    )
  }, error = function(errorCondition) {
    message(errorCondition)
    check.call(
      c(
        getOption("ie.webui.docker-machine", "docker-machine"), "rm", "-f",
        machineName
      )
    )
    signalCondition(errorCondition)
  })
}

# Shut down the worker
dismissWorker = function(worker) {
  if(worker$local) {
    dismissLocalWorker(worker)
  } else {
    dismissRemoteWorker(worker)
  }
}

dismissLocalWorker = function(worker) {
}

dismissRemoteWorker = function(worker) {
  check.call(
    c(
      getOption("ie.webui.docker-machine", "docker-machine"), "rm", "--force",
      worker$machineName
    )
  )
}

check.call = function(args) {
  status = system2(args[1], args[2:length(args)], stdout="", stderr="")
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return()
  } else {
    stop(sprintf("status = %d", status))
  }
}

check.output = function(args) {
  if (length(args) > 1) {
    res = system2(args[1], args[2:length(args)], stdout=TRUE, stderr="")
  } else {
    res = system2(args[1], c(), stdout=TRUE, stderr="")
  }
  status = attr(res, "status", exact=TRUE)
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return(res)
  } else {
    stop(sprintf("status = %d", status))
  }
}

