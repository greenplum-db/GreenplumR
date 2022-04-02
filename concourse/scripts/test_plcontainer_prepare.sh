#!/bin/bash -l
set -eox pipefail
# install dependencies of GreenplumR

CWDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_DIR=${CWDIR}/../../../
# light or full
MODE=${MODE:=light}
# for now we specific R version
R_VERSION=3.6.1

function determine_os() {
    if [ -f /etc/redhat-release ] ; then
      echo "centos"
      return
    fi
    if grep -q ID=ubuntu /etc/os-release ; then
      echo "ubuntu"
      return
    fi
    echo "Could not determine operating system type" >/dev/stderr
    exit 1
}

function install_libraries() {
    install_libraries_${MODE}
}
function install_libraries_min() {
    # install system libraries
    TEST_OS=$(determine_os)
    case $TEST_OS in
    centos)
      yum install -y epel-release
      # postgresql-devel is needed by RPostgreSQL
      yum install -y postgresql-devel
      LINUX_VERSION=$(rpm -E %{rhel})
      curl -O https://cdn.rstudio.com/r/centos-${LINUX_VERSION}/pkgs/R-${R_VERSION}-1-1.x86_64.rpm
      yum install -y R-${R_VERSION}-1-1.x86_64.rpm
      ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
      ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript
      ;;
    ubuntu)
      apt update
      DEBIAN_FRONTEND=noninteractive apt install -y r-base libpq-dev build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
      ;;
    *)
      echo "unknown TEST_OS = $TEST_OS"
      exit 1
      ;;
    esac
    # install r libraries
    Rscript ${CWDIR}/install_r_package.R remotes
    Rscript ${CWDIR}/install_r_package.R testthat
    Rscript ${CWDIR}/install_r_package.R shiny
    Rscript ${CWDIR}/install_r_package.R ini

    # install r libraries from GitHub
    Rscript ${CWDIR}/install_r_package_github.R tomoakin/RPostgreSQL/RPostgreSQL
}

function install_libraries_full() {
  install_libraries_min
  # install system libraries
  case $TEST_OS in
  centos)
    # no more packages need to install
    ;;
  ubuntu)
    DEBIAN_FRONTEND=noninteractive apt install -y pkg-config \
        texlive-latex-base texlive-fonts-extra
    ;;
  esac
  # install additional r libraries
}

function install_libraries_light() {
    install_libraries_min
    tar czf ${TOP_DIR}/GreenplumR.tar.gz ${TOP_DIR}/GreenplumR_src
    R CMD INSTALL ${TOP_DIR}/GreenplumR.tar.gz
}

# install libraries (light/full)
function _main() {
    time install_libraries
}

_main "$@"

