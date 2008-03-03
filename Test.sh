#!/bin/bash
# Running the test suite requires installing Test::Run with the 
# AlternateInterpreters plugin. See:
# 
# * http://search.cpan.org/perldoc?Task::Test::Run::AllPlugins
# * http://web-cpan.berlios.de/modules/Test-Run/
# 
export HARNESS_ALT_INTRP_FILE="$(pwd)/t/files/interpreters.conf.yml"
if echo "$HARNESS_PLUGINS" | grep -q -E '(^| )AlternateInterpeters( |$)' ; then
    export HARNESS_PLUGINS="$HARNESS_PLUGINS AlternateInterpeters"
fi
runprove t/*.t
