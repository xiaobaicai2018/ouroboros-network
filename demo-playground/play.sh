#!/bin/sh

XGEOM="124x40"

CWD=`pwd`

NODECOMMAND="cd ${CWD}; cabal new-run demo-playground -- node -t demo-playground/simple-topology.json -n "
SBMTCOMMAND="cd ${CWD}; cabal new-run demo-playground -- submit -t demo-playground/simple-topology.json -n "

NODE=0
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE}" &
NODE=1
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE}" &
NODE=2
xterm -name "node${NODE}" -geometry $XGEOM -e "${NODECOMMAND} ${NODE}" &


echo "open localhost:8000"

#sleep 5

#xterm -name "submission" -geometry $XGEOM -e "${SBMTCOMMAND} 2 --address a --amount 1000" &

