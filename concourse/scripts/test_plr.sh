#!/bin/bash -l
set -exo pipefail

OLDPATH=${PATH}
echo "OLDPATH = ${OLDPATH}"
CWDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_DIR=${CWDIR}/../../../
GPDB_CONCOURSE_DIR=${TOP_DIR}/gpdb_src/concourse/scripts
# light or full
MODE=${MODE:=light}
# for now we specific R version
R_VERSION=3.6.1

source "${GPDB_CONCOURSE_DIR}/common.bash"

function test_run() {
  cat > /home/gpadmin/test_prepare.sh <<-EOF
#!/bin/bash -l
# install plr and restart gpdb
# then prepare databases needed by test
set -exo pipefail
source /usr/local/greenplum-db-devel/greenplum_path.sh
source ${TOP_DIR}/gpdb_src/gpAux/gpdemo/gpdemo-env.sh
gppkg -i ${TOP_DIR}/bin_plr/plr-*.gppkg
sleep 1
source /usr/local/greenplum-db-devel/greenplum_path.sh
source ${TOP_DIR}/gpdb_src/gpAux/gpdemo/gpdemo-env.sh
gpstop -arf
createdb rtest
createdb debug_apply
createdb d_apply
createdb d_tapply
EOF
  cat > /home/gpadmin/test_run.sh <<-EOF
#!/bin/bash -l
set -exo pipefail
export GPRLANGUAGE=plr
pushd ${TOP_DIR}/GreenplumR_src
  # clear environment introduced by gpdb that may affect R
  sleep 3
  export PGPORT=6000
  export PATH=${OLDPATH}
  unset R_HOME
  unset R_LIBS_USER
  unset LD_LIBRARY_PATH
  if [ "$MODE" == "light" ] ; then
    echo "library(testthat)" > test_script.R
    echo "testthat::test_dir('tests', reporter = 'fail', stop_on_failure = TRUE)" >> test_script.R
    R --no-save < test_script.R
  else
    R CMD check . --no-manual
  fi
popd
EOF

  chown -R gpadmin:gpadmin $(pwd)
  pushd /home/gpadmin
    chown gpadmin:gpadmin test_prepare.sh test_run.sh
    chmod a+x test_prepare.sh test_run.sh
  popd
  su gpadmin -c "/bin/bash /home/gpadmin/test_prepare.sh"
  su gpadmin -c "/bin/bash /home/gpadmin/test_run.sh"
}

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

function setup_gpadmin_user() {
    ${GPDB_CONCOURSE_DIR}/setup_gpadmin_user.bash
}

function install_libraries() {
    # install system libraries
    case $TEST_OS in
    centos)
      yum install -y epel-release
      # postgresql-devel is needed by RPostgreSQL
      yum install -y postgresql-devel
      # install R specific
      LINUX_VERSION=$(rpm -E %{rhel})
      curl -O https://cdn.rstudio.com/r/centos-${LINUX_VERSION}/pkgs/R-${R_VERSION}-1-1.x86_64.rpm
      yum install -y R-${R_VERSION}-1-1.x86_64.rpm
      ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
      ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript
      ;;
    ubuntu)
      apt update
      DEBIAN_FRONTEND=noninteractive apt install -y r-base libpq-dev
      ;;
    *)
      echo "unknown TEST_OS = $TEST_OS"
      exit 1
      ;;
    esac
    # install r libraries use remotes to download GitHub packages
    Rscript ${CWDIR}/install_r_package.R remotes
    Rscript ${CWDIR}/install_r_package.R testthat
    Rscript ${CWDIR}/install_r_package.R shiny
    Rscript ${CWDIR}/install_r_package.R ini
    # install r libraries from GitHub
    Rscript ${CWDIR}/install_r_package_github.R tomoakin/RPostgreSQL/RPostgreSQL
}

function install_libraries_full() {
  install_libraries
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
    install_libraries
    tar czf ${TOP_DIR}/GreenplumR.tar.gz ${TOP_DIR}/GreenplumR_src
    R CMD INSTALL ${TOP_DIR}/GreenplumR.tar.gz
}

# install libraries (light/full)
# install gpdb
# setup gpadmin
# make cluster
# install plr/plcontainer
# restart gpdb
# clear environment introduced by gpdb
#
# run tests (light/full)
function _main() {
    TEST_OS=$(determine_os)
    time install_libraries_${MODE}
    time install_gpdb
    time setup_gpadmin_user

    # for mirrorless test
    export WITH_MIRRORS=false
    export WITH_STANDBY=false
    export BLDWRAP_POSTGRES_CONF_ADDONS=wal_level=minimal,max_wal_senders=0,gp_dispatch_keepalives_idle=30,gp_dispatch_keepalives_interval=10,gp_dispatch_keepalives_count=4
    time make_cluster

    time test_run
}

_main "$@"
