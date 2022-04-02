#!/bin/bash -l
set -eox pipefail

# light or full
MODE=${MODE:=light}

function test_run() {
  cat > /home/gpadmin/test_run.sh <<-EOF
#!/bin/bash -l
set -exo pipefail
export GPRLANGUAGE=plcontainer
pushd ~/GreenplumR_src
  if [ "$MODE" == "light" ] ; then
    echo "library(testthat)" > test_script.R
    echo "testthat::test_dir('tests', reporter = 'fail', stop_on_failure = TRUE)" >> test_script.R
    R --no-save < test_script.R
  else
    R CMD check . --no-manual
  fi
popd
EOF

  chmod a+x /home/gpadmin/test_run.sh
  bash /home/gpadmin/test_run.sh
}

# run tests (light/full)
function _main() {
    time test_run
}

_main "$@"
