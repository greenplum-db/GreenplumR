#!/bin/bash
set -exo pipefail
# this script is used to quickly run tests when pipeline failed
CWDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
MODE=${MODE} PGPORT=5432 bash ${CWDIR}/run_test_plcontainer.sh
