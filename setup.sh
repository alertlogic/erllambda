#!/usr/bin/env bash
SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# ensure dev_scripts is available and up-to-date
mkdir -p _build
for comp in dev_scripts makeincl; do
    if [[ ! -d _build/${comp} ]]; then
        (cd _build && \
         git clone git@algithub.pd.alertlogic.net:alertlogic/${comp}.git)
    else
        (cd _build/${comp} && \
         git fetch origin && git pull origin master)
    fi
done

# choose correct dev_scripts location
DEV_SCRIPTS_DIR=_build/dev_scripts
if [[ -e _checkouts/dev_scripts ]]; then
    DEV_SCRIPTS_DIR=_checkouts/dev_scripts
fi
SCRIPT_DIR="${SCRIPT_DIR}" COMPS_NEEDED="${COMPS_NEEDED}" \
${DEV_SCRIPTS_DIR}/setup.sh "$@"
