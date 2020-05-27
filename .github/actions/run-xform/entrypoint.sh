#!/bin/bash
###############################################################################
# Docker container entrypoint script.
###############################################################################

XFORM_COMMAND=$@

if [ -z "${GITHUB_WORKSPACE}" ]; then
  echo "GITHUB_WORKSPACE not set, aborting."
  exit 1
fi

if [ -z "${XFORM_COMMAND}" ]; then
  echo "xform-command not provided, aborting."
  exit 1
fi

cd "${GITHUB_WORKSPACE}"

.github/actions/run-xform/install.sh
INSTALL_STATUS=$?

if [ ${INSTALL_STATUS} -ne 0 ]; then
  echo "Install failed."
  exit 1
fi

echo "Executing: ${XFORM_COMMAND}"
eval ${XFORM_COMMAND}
XFORM_STATUS=$?

if [ ${XFORM_STATUS} -eq 0 ]; then
  echo "Success."
else
  echo "Fail."
fi

exit ${XFORM_STATUS}
