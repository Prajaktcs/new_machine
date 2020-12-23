export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
export PATH=$(go env GOPATH)/bin:~/bin:$PATH

export LANG="en_US.UTF-8"

alias nv="nvim"
alias vagrant_restart="vagrant destroy -f && vagrant up" 
export VAULT_ADDR="http://vault.vividseats.test:8200/"
export VAULT_TOKEN="90c97305-f9c4-c7cb-c52b-594c3a7d9ef0"

function git_delete_merged() {
    git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
    echo "Pruning"
    git remote prune origin
}

function copy_airflow_dags() {
    # Run this function from the Projects directory
    cp -R airflow-dags/dags/* airflow-deployment/dags/
}

source ~/.local/share/icons-in-terminal/icons_bash.sh

function docker_kill_and_rm() {
    docker kill $(docker ps -a -q)
    docker rm $(docker ps -a -q)
}



#complete -C /usr/local/bin/mc mc

# added by Snowflake SnowSQL installer v1.0
export PATH=/Applications/SnowSQL.app/Contents/MacOS:$PATH

function kill_process_on_port() {
    kill -9 $(lsof -i TCP:$1 | grep LISTEN | awk '{print $2}')
}

function co_master_pull() {
    git checkout master
    git pull
}

function tmux_list_sessions() {
    tmux ls && read tmux_session && tmux attach -t ${tmux_session:-default} || tmux new -s ${tmux_session:-default}
}

function tunnel_to_database() {
    # Example usage - tunnel_to_database {local port} {database endpoint} {remote port}
    ssh -L $1:$2:$3 prajakt.shastry@data-bastion
}


function find_and_replace() {
    $folderName = $1
    $fileType = $2
    $find = $3
    $replace = $4

    find $folderName -name '*.$fileType' -type f -exec sed -i -e 's/$find/$replace/g'  {} +
}


alias gitco="git checkout"
alias vim="nvim"


