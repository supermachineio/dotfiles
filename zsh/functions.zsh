function docker_cleanup_images() {
  docker images | \
    grep -e "^<none>" | \
    awk '{print $3}' | \
    xargs docker rmi -f
}

function docker_cleanup_containers() {
  docker ps -a | \
    grep Exit | \
    cut -d ' ' -f 1 | \
    xargs docker rm
}

function blockbuild() {
  docker-compose kill; \
    docker-compose down; \
    sudo rm -rf tmp && \
    ./install local-to-docker && \
    stratoHost=nginx genesisBlock="$(cat gb.json)" ./strato-admin.sh --run-local
}
