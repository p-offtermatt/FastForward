#!/bin/sh

###
## ProM specific
###
PROGRAM=ProM
CP=./dist/ProM-Framework-6.11.121.jar:./dist/ProM-Contexts-6.11.67.jar:./dist/ProM-Models-6.10.43.jar:./dist/ProM-Plugins-6.9.75.jar
LIBDIR=./lib
MAIN=org.processmining.contexts.cli.CLI

####
## Environment options
###
JAVA=java

###
## Main program
###

add() {
	CP=${CP}:$1
}


for lib in $LIBDIR/*.jar
do
	add $lib
done

$JAVA\
 -Xmx3G\
 -da\
 -classpath ${CP}\
 -Djava.library.path=${LIBDIR}\
 -Djava.system.class.loader=org.processmining.framework.util.ProMClassLoader\
 -Djava.util.Arrays.useLegacyMergeSort=true\
 ${MAIN} $1 $2
