#!/bin/bash

function print_info()
{
  yellow='\033[0;33m'
  nocolor='\033[0m'

  printf "${yellow}"
  printf "$1"
  printf "${nocolor}"
}

function print_success()
{
  green='\033[0;32m'
  nocolor='\033[0m'

  printf "${green}"
  printf "$1"
  printf "${nocolor}"
}

# Run a one-off command inside a running container
# Function
#   mexec
# Arguments:
#   mexec [SERVICE_NAME] [CONTAINER_COMMAND]
# Examples:
#   mexec fundraising bundle exec rspec ./spec/whatever_spec.rb
#   mexec launch yarn install
mexec () {
  dc exec "$@"
}

# Run a one-off command inside a new container
# Function
#   mrun
# Arguments:
#   mrun [SERVICE_NAME] [CONTAINER_COMMAND]
# Examples:
#   mrun fundraising bundle exec rspec ./spec/whatever_spec.rb
#   mexunch yarn install
mrun () {
  dc run --rm "$@"
}


# Rebuild containers
# Function
#   mrebuild
# Arguments:
#   mrebuild [CONTAINER_NAMES]
# Examples:
#   mrebuild fundraising launch metarepo-reverse-proxy
mrebuild () {
  container_names=$@

  if [[ -z "${container_names// }" ]]
  then
    echo "Missing container name argument"
  else
    dc rm -sf $container_names
    dc build $container_names
    dc up -d $container_names
  fi
}

# shortcut for docker-compose -f /home/ubuntu/metarepo/docker-compose.yml
# Function
#   dc
# Arguments:
#   dc [...rest of your docker-compose command]
# Examples:
#   dc exec fundriasing bash
#   dc run launch yarn install
dc () {
  if [[ -f "$PWD/docker-compose.yml" ]]
  then
    dc_context="docker-compose"
  else
    dc_context="docker-compose -f /home/ubuntu/metarepo/docker-compose.yml"
  fi

  dc_cmd=$1
  dc_cmd_args=${@:2}
  terminal_height=$(tput lines)
  terminal_width=$(tput cols)

  if [[ $1 == "run" || $1 == "exec" ]]
  then
    # Inject the callers terminal dimensions into the container
    # as environment variables.
    # This solves an issues where container terminals artificially
    # wrap at 80 chars.
    $dc_context $dc_cmd \
      -e LINES=$terminal_height \
      -e COLUMNS=$terminal_width \
      -e SHELL=/bin/bash \
      $dc_cmd_args
  else
    $dc_context $@
  fi
}
