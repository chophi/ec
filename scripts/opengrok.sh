#!/bin/bash

unset JAVA_HOME

if [ `uname` == "Linux" ]; then
    JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/
    memory_setting=-Xmx4096m
else
    JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.*/Contents/Home
    memory_setting=-Xmx4096m
fi

export PATH=$JAVA_HOME/bin:$PATH
echo "PATH is [$PATH]"
echo "JAVA is from: `which java`"
echo "Memory limit: $memory_setting"

opengrok_lib=$HOME/repo/public/tools/opengrok-bin/opengrok-1.1-rc18/lib/opengrok.jar
ctags=/usr/local/bin/ctags
class=org.opensolaris.opengrok.search.Search

first_arg=$1
echo Parameters are: [$@]
shift
if [ "$first_arg" == "index" ]; then
    mkdir -p ~/logs/opengrok/
    log_file=~/logs/opengrok/index-`date +"%Y.%m.%d-%H.%M.%S"`.log
    echo "Log file: $log_file"
    time java $memory_setting \
         -jar $opengrok_lib \
         -c $ctags \
         $@ \
         1>$log_file \
         2>$log_file
    echo "Index completed"
elif [ "$first_arg" == "search_full_text" ]; then
    commands=""
    next=""
    for arg in "$@"; do
        commands=${commands}${next}
        next=" "$arg
        last=$arg
    done
    echo Search command is: [java $memory_setting -cp $opengrok_lib $class $commands \""$last"\"
    time java $memory_setting -cp $opengrok_lib $class $commands\""$last"\"
elif [ "$first_arg" == "search" ]; then
    echo Search command is: [java $memory_setting -cp $opengrok_lib $class $@]
    time java $memory_setting -cp $opengrok_lib $class $@
fi
