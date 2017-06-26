#!/bin/bash -e
# Install hres/cvapps to a shiny server
###############################################################################
set -e          # exit on command errors (so you MUST handle exit codes properly!)
set -E          # pass trap handlers down to subshells
set -o pipefail # capture fail exit codes in piped commands
#set -x         # execution tracing debug messages

# Error handler
on_err() {
	echo ">> ERROR: $?"
	FN=0
	for LN in "${BASH_LINENO[@]}"; do
		[ "${FUNCNAME[$FN]}" = "main" ] && break
		echo ">> ${BASH_SOURCE[$FN]} $LN ${FUNCNAME[$FN]}"
		FN=$(( FN + 1 ))
	done
}
trap on_err ERR

# Exit handler
declare -a EXIT_CMDS
add_exit_cmd() { EXIT_CMDS+="$*;  "; }
on_exit(){ eval "${EXIT_CMDS[@]}"; }
trap on_exit EXIT

# Get command info
CMD_PWD=$(pwd)
CMD="$0"
CMD_DIR="$(cd "$(dirname "$CMD")" && pwd -P)"

# Defaults and command line options
#[ "$VERBOSE" ] ||  VERBOSE=
#[ "$DEBUG" ]   ||  DEBUG=
[ "$SHINY_SRV_DIR" ]   ||  SHINY_SRV_DIR=/srv/shiny-server
[ "$SHINY_USER" ]   ||  SHINY_USER=shiny:shiny


# Basic helpers
out() { echo "$(date +%Y%m%dT%H%M%SZ): $*"; }
err() { out "$*" 1>&2; }
vrb() { [ ! "$VERBOSE" ] || out "$@"; }
dbg() { [ ! "$DEBUG" ] || err "$@"; }
die() { err "EXIT: $1" && [ "$2" ] && [ "$2" -ge 0 ] && exit "$2" || exit 1; }

# Show help function to be used below
show_help() {
	awk 'NR>1{print} /^(###|$)/{exit}' "$CMD"
	echo "USAGE: $(basename "$CMD") [arguments]"
	echo "ARGS:"
	MSG=$(awk '/^NARGS=-1; while/,/^esac; done/' "$CMD" | sed -e 's/^[[:space:]]*/  /' -e 's/|/, /' -e 's/)//' | grep '^  -')
	EMSG=$(eval "echo \"$MSG\"")
	echo "$EMSG"
}

# Parse command line options (odd formatting to simplify show_help() above)
NARGS=-1; while [ "$#" -ne "$NARGS" ]; do NARGS=$#; case $1 in
	# SWITCHES
	-h|--help)      # This help message
		show_help; exit 1; ;;
	-d|--debug)     # Enable debugging messages (implies verbose)
		DEBUG=$(( DEBUG + 1 )) && VERBOSE="$DEBUG" && shift && echo "#-INFO: DEBUG=$DEBUG (implies VERBOSE=$VERBOSE)"; ;;
	-v|--verbose)   # Enable verbose messages
		VERBOSE=$(( VERBOSE + 1 )) && shift && echo "#-INFO: VERBOSE=$VERBOSE"; ;;
	# PAIRS
	-t|--thing)     # Set a thing to a value (DEFAULT: $THING)
		shift && THING="$1" && shift && vrb "#-INFO: THING=$THING"; ;;
	*)
		break;
esac; done

[ "$DEBUG" ]  &&  set -x

###############################################################################

# Validate some things
#TODO: You will probably want to change this but this is an example of simple params validation
#[ $# -gt 0 -a -z "$THING" ]  &&  THING="$1"  &&  shift
#[ "$THING" ]  ||  die "You must provide some thing!"
#[ $# -eq 0 ]  ||  die "ERROR: Unexpected commands!"

cp -a apps/CVShiny/. $SHINY_SRV_DIR/CVShiny_dev/
cp apps/common_ui.R $SHINY_SRV_DIR/shinyfaers_dev/
cp apps/common_ui.R $SHINY_SRV_DIR/shinydisp2_dev/

cp apps/faers.R $SHINY_SRV_DIR/shinyfaers_dev/app.R
cp apps/disp.R $SHINY_SRV_DIR/shinydisp2_dev/app.R

chown -R $SHINY_USER $SHINY_SRV_DIR/CVShiny_dev/
chown -R $SHINY_USER $SHINY_SRV_DIR/shinyfaers_dev/
chown -R $SHINY_USER $SHINY_SRV_DIR/shinydisp2_dev/
 