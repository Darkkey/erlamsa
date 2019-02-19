#!/bin/sh

PROCESS_NAME=$1
ERLAMSA_IP=$2
ERLAMSA_PORT=$3

if [ "$#" -ne 3 ]; then
	echo "Simple process monitor script for erlamsa"
	echo ""
    echo "Illegal number of parameters. Usage $0 <PROCESS_NAME> <ERLAMSA_IP> <ERLAMSA_PORT>"
	echo ""
	exit
fi

while :
do
	IS_RUNNING=`ps -aef | grep -i "${PROCESS_NAME}" | grep -v 'grep' | grep -v 'simple_process_monitor' | awk '{ print $3 }'`
	STRLEN=${#IS_RUNNING}
	if [ $STRLEN -eq 0 ]
	then
		MSG="{event} Process ${PROCESS_NAME} is no longer running on the system."
		echo $MSG | nc $ERLAMSA_IP $ERLAMSA_PORT
	fi
	sleep 1
done

