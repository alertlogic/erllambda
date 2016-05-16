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

SCRIPT_DIR="${SCRIPT_DIR}" COMPS_NEEDED="${COMPS_NEEDED}" \
_build/dev_scripts/setup.sh "$@"
