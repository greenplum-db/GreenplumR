#!/bin/bash

#------------------------------------------------------------------------------
#
# Copyright (c) 2017-Present Pivotal Software, Inc
#
#------------------------------------------------------------------------------

set -exo pipefail

CWDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_DIR=${CWDIR}/../../../

release_greenplumr() {

  # Get GreenplumR version
  pushd GreenplumR_src
  if git describe --tags >/dev/null 2>&1 ; then
      echo "git describe failed" || exit 1
  fi
  GREENPLUMR_VERSION=$(git describe --tags | awk -F. '{printf("%d.%d", $1, $2)}')
  GREENPLUMR_RELEASE=$(git describe --tags | awk -F. '{print $3}')
  popd

  # release GreenplumR
  mkdir -p greenplumR-release
  tar zcf ${TOP_DIR}/greenplumR.tar.gz ${TOP_DIR}/GreenplumR_src
  cp greenplumR.tar.gz greenplumR-release/greenplumR-${GREENPLUMR_VERSION}.${GREENPLUMR_RELEASE}-gp6.tar.gz
}

release_greenplumr
