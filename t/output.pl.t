use strict;
use warnings;

use Test::More tests => 1;

use Test::Trap qw( trap $trap :flow:stderr(systemsafe):stdout(systemsafe):warn );

trap {
    system("bash", "arc.sh", "t/files/ero1.arc");
};

# TEST
is ($trap->stderr(), qq{"Hello" "World"\n}, "Checking for correct stderr");
