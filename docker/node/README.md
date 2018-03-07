## Docker Compose RChain Node using Prometheus & Grafana UI 
RChain docker compose for peer-to-peer node using Prometheus for data collection and Grafana for visualization 

### Prerequisites
You need `docker-compose` that supports 3.1 docker-compose version - checkout [install guide](https://docs.docker.com/compose/install/)
and docker - checkout [Docker Community Edition](https://www.docker.com/community-edition)

### Usage
```
from this directory run
docker-compose up -d

remove all containers and volumes
docker-compose down -v 


docker-compose up -d
docker-compose down
docker-compose stop 
docker-compose start 
docker-compose down -v  # removes docker volumes deleting all previous data
```

Where 127.0.0.1:9091 is socket of your Prometheus Pushgateway
echo "some_metric 10" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job1
echo "some_metric_2 30" | curl --data-binary @- http://127.0.0.1:9091/metrics/job/job2
curl http://127.0.0.1:9091/metrics


### Troubleshooting docker images from shell
```
docker exec --user root -it <yourdockerimage> /bin/sh
```

### Other Common Commands
```
docker container list
docker container list --all
docker volume list
docker image list
docker image rm <my image>
```

Example of pushing modified docker to your own repo
#!/usr/bin/env bash
DOCKER_USERNAME="yourusername"
DOCKER_PASSWORD="yourpassword"
image_id="imageid"
image_tag="rchain-node:dev"
repo="${DOCKER_USERNAME}/${image_tag}"
docker tag ${image_id} $repo
docker login -u ${DOCKER_USERNAME} -p ${DOCKER_PASSWORD}
docker push $repo

### References
https://docs.docker.com/compose/compose-file/
https://docs.docker.com/
