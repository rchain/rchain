## RChain Node(RNode) Docker Build & Optional Push 
This script can be used with [Docker Community Edition](https://www.docker.com/community-edition) in order do local docker builds of RChain platform and the creation of the docker image of the peer-to-peer node. Optionally, you can push the Docker image you create to [Docker Hub](https://hub.docker.com/) or the repo of your choice. This can also be a convenient way to test your branch locally without using a CI or continuous integration platform.

### Working Docker Enviroments 
Every Docker enviroment can have its own unique settings. This has been tested on Docker running on Linux. You will always need the appropriate permissions for whatever you are doing in Docker.
If you are running Docker CE on Windows you will need to "share your drive" on your host through **Docker->Settings->Shared Drives** and then make sure the drive that your container information resides on is checked. We will have more support and detailed documentation for all major platforms in the future. Right now, for ease of use and support we recommend using Docker on Linux host when possible. The conevenience of using containers is absolutely no residual data issues pre or post build. You can always keep Docker images if you want or remove them.

### Prerequisites
You will need a `docker` version that supports the needed functionality of this application - checkout [Docker Community Edition](https://www.docker.com/community-edition) - The lastest stable version of CE will always work.

You may also want to install `docker-compose` version that supports Docker Compose file format version 3.1 or greater - Easily install single file with latest version from [install docker-compose](https://docs.docker.com/compose/install/)

### Quick Start 

In the directory of this README run 
```./run.sh
```
Docker and the script will take care of the rest. From the code you can see that it creates a Ubuntu 16.04 LTS container and runs the "rchain-docker-build-push.sh" script in it. 
