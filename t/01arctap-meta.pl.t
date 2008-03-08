use strict;
use warnings;

use Test::More tests => 1;

use Test::Trap qw( trap $trap :flow:stderr(systemsafe):stdout(systemsafe):warn );

trap {
    system("bash", "arc.sh", "t/files/tap-diag1.arc");
};

# TEST
is ($trap->stderr(), qq{# Hi\n# There\n}, "Checking for correct diag");

