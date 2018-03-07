## Docker Compose RChain Node using Prometheus & Grafana UI 
RChain docker compose for peer-to-peer node using Prometheus for data collection and Grafana for visualization 

### Prerequisites
You need `docker-compose` that supports version 3.1 or greater docker-compose files - checkout [install docker-compose guide](https://docs.docker.com/compose/install/)

You also need `docker` version that supports the needed functionality - checkout [Docker Community Edition](https://www.docker.com/community-edition)


### Quick Start 
```
# Run these commands from this directory 
docker-compose up -d

Login to http://localhost:3000/dashboards on you machine using user:pass admin:admin
Click on RChain dashboard (4 squares by name)

# Remove all containers and associated volumes - volumes store persistent data
docker-compose down -v 
```

### Quick Start Notes:
```
Grafana preferences are set to refresh every 30 seconds. You can change this to your preference. The same with color scheme. 
We "exposed" docker container tcp port 3000 so Grafana should be accessible from your browser
We've exposed other ports for access convenience from your host. Make sure you always run this behind a firewall or you would want to change these settings and default password 
The password can be changed by editing grafana/grafana.conf
```

### Docker-Compose Usage
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

### Login to Grafana
```
Go to http://localhost:3000 on your machine
We exposed tcp 3000 so it should be accessible from your hosts browser
```

### Other Docker Compose Commands
```
https://docs.docker.com/compose/reference/ 
```

### Sending Metrics to Promethus Pushgateway Using Curl 
```
# You can use any http client library to send data. Push gateway is collected via Prometheus
# Where 127.0.0.1:9091 is socket of your Prometheus Pushgateway
echo "some_metric 10" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job1
echo "some_metric_2 30" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job2
curl http://127.0.0.1:9091/metrics
```


### Troubleshooting Docker Container from Shell
```
docker exec --user root -it <your-docker-container-id> /bin/sh
```

### View Docker Container Logs
```
docker logs <your-docker-container-id>
```

### Other Common Docker Commands
```
docker container list
docker container list --all
docker volume list
docker image list
docker image rm <my image>
```

### Push Docker Example 
```
#!/usr/bin/env bash
# Make changes to your docker image and push to your own repo
# Example of pushing modified docker to your own repo
DOCKER_USERNAME="yourusername"
DOCKER_PASSWORD="yourpassword"
image_id="imageid"
image_tag="rchain-node:dev"
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
