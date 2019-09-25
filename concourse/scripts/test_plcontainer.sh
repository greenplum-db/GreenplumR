#!/bin/bash -l
set -exo pipefail

ccp_src/scripts/setup_ssh_to_cluster.sh
#############################################################
# install docker
#############################################################
plcontainer_src/concourse/scripts/docker_install.sh

#############################################################
# install plcontainer
#############################################################

scp -r bin_plcontainer mdw:/tmp/

ssh mdw "bash -c \" \
set -eox pipefail; \
export MASTER_DATA_DIRECTORY=/data/gpdata/master/gpseg-1; \
source /usr/local/greenplum-db-devel/greenplum_path.sh; \
gppkg -i /tmp/bin_plcontainer/plcontainer*.gppkg; \
\""


#############################################################
# install docker image
#############################################################
scp -r plcontainer_rclient_docker_image/plcontainer-*.tar.gz \
    mdw:/usr/local/greenplum-db-devel/share/postgresql/plcontainer/plcontainer-r-images.tar.gz

ssh mdw "bash -c \" \
set -eox pipefail; \
export MASTER_DATA_DIRECTORY=/data/gpdata/master/gpseg-1; \
source /usr/local/greenplum-db-devel/greenplum_path.sh; \
plcontainer image-add -f /usr/local/greenplum-db-devel/share/postgresql/plcontainer/plcontainer-r-images.tar.gz; \
plcontainer runtime-add -r plc_r_shared -i pivotaldata/plcontainer_r_shared:devel -l r -s use_container_logging=yes; \
gpstop -arf; \
createdb rtest; \
createdb d_apply; \
createdb d_tapply; \
\""
#############################################################
# run tests
#############################################################
scp -r GreenplumR_src mdw:~/

# install dependencies
case "$platform" in
centos*)
    node=centos
    ;;
ubuntu*)
    node=ubuntu
    ;;
*)
    echo "unknown platform: $platform"
    exit 1
    ;;
esac

ssh $node@mdw "sudo bash -c \" \
set -eox pipefail; \
export MODE=${MODE}; \
bash /home/gpadmin/GreenplumR_src/concourse/scripts/test_plcontainer_prepare.sh; \
\""

ssh mdw "bash -c \" \
set -eox pipefail; \
export PGPORT=5432; \
export MODE=${MODE}; \
pushd GreenplumR_src; \
bash concourse/scripts/run_test_plcontainer.sh
popd; \
\""
#############################################################
