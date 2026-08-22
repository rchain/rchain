## RChain Node with Metric Collection and Display 
RChain Docker Compose multi-container application for RChain peer-to-peer node using [Prometheus Server](https://github.com/prometheus/prometheus) and [Pushgateway](https://github.com/prometheus/pushgateway) for data collection and [Grafana](https://github.com/grafana/grafana) for visualization 

### Working Docker Environments
Every Docker environment can have its own unique settings. This has been tested on Docker running on Linux. You will always need the appropriate permissions for whatever you are doing in Docker.
If you are running Docker CE on Windows you will need to "share your drive" on your host through **Docker->Settings->Shared Drives** and then make sure the drive that your container information resides on is checked. We will have more support and detailed documentation for all major platforms in the future. Right now, for ease of use and support we recommend using Docker on Linux when possible.

### Prerequisites
You need `docker-compose` version that supports Docker Compose file format version 3.1 or greater - Easily install single file with latest version from [install docker-compose](https://docs.docker.com/compose/install/)

You will also need a `docker` version that supports the needed functionality of this application - checkout [Docker Community Edition](https://www.docker.com/community-edition) - The latest stable version of CE will always work.


### Quick Start 
```
# Run commands from this directory - contains node docker-compose.yml
docker-compose up -d
```

* Login to http://localhost:3000/dashboards on you machine using user:pass admin:admin
* Click on RChain dashboard (4 squares by name)
* View data. [Add another panel in dashboard and query](http://docs.grafana.org/guides/getting_started/#adding-editing-graphs-and-panels) as wanted.
 
```
# Remove all containers and associated volumes - volumes store persistent data
docker-compose down -v 
```

### Quick Start Notes
Grafana preferences are set to refresh every 30 seconds. You can change this to your preference. The same with color scheme. 
We "exposed" docker container tcp port 3000 so Grafana should be accessible from your browser
We've exposed other ports for access convenience from your host. Make sure you always run this behind a firewall or you would want to change these settings and default password 
The password for Grafana UI can be changed by editing grafana/grafana.conf

### Sending Metrics to Promethus Pushgateway Using Curl 
You can use any http client library to send data. Pushgateway stats are collected via Prometheus Server. More info on [when to use the pushgateway](https://prometheus.io/docs/practices/pushing/)<br>
Where 127.0.0.1:9091 is socket of your Prometheus Pushgateway
```
echo "some_metric 10" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job1
echo "some_metric_2 30" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job2
curl http://127.0.0.1:9091/metrics
```

## More Docker Commands
The rest of documentation is helpful docker and docker-compose commands for managing docker resources for this application.

### Docker-Compose Usage
More docker-compose command usage
```
# Run these commands from this directory 
# Docker containers will have "node_" in front of them from this directory name

# Run multi-container docker-compose app in detached mode
docker-compose up -d

# See running containers
docker container list

# Restart containers 
docker-compose restart

# Stop containers 
docker-compose stop 

# Remove all containers and associated volumes - volumes store persistent data
docker-compose down -v 
```

### Other Docker Compose Commands
```
https://docs.docker.com/compose/reference/ 
```

### Troubleshooting Docker Container from Shell
```
docker exec --user root -it <your-docker-container-id> /bin/sh
```

### View Docker Container Logs
```
docker logs <your-docker-container-id>
```

### Other Common Docker Commands Used for Resource Management
```
docker container list
docker container list --all
docker volume list
docker image list
docker image rm <my image>
```

### Simple Push Docker Example Using BASH
```
#!/usr/bin/env bash
# Make changes to your docker image and push to your own repo
# Example of pushing modified docker to your own repo
DOCKER_USERNAME="yourusername"
DOCKER_PASSWORD="yourpassword"
image_id="imageid"
image_tag="some-name:test"
repo="${DOCKER_USERNAME}/${image_tag}"
docker tag ${image_id} $repo
docker login -u ${DOCKER_USERNAME} -p ${DOCKER_PASSWORD}
docker push $repo
```

### References
```
https://docs.docker.com/compose/compose-file/
https://docs.docker.com/compose/reference/ 
https://docs.docker.com/
```
