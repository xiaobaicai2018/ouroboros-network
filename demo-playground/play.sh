#!/bin/sh

XGEOM="124x40"

CWD=`pwd`

BP="demo-playground"
NODECOMMAND="cd ${CWD}; cabal new-run demo-playground -- node -t ${BP}/simple-topology.json -n "
SBMTCOMMAND="cd ${CWD}; cabal new-run demo-playground -- submit -t ${BP}/simple-topology.json -n "

NODE=0
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE} -l ${BP}/log-config-${NODE}.yaml" &
NODE=1
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE} -l ${BP}/log-config-${NODE}.yaml" &
NODE=2
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE} -l ${BP}/log-config-${NODE}.yaml" &


echo "open localhost:12788"
echo "open localhost:12789"
echo "open localhost:12790"

#sleep 5

#xterm -name "submission" -geometry $XGEOM -e "${SBMTCOMMAND} 2 --address a --amount 1000" &

