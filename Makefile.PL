use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Games::Cards',
    'VERSION_FROM' => 'Games/Cards.pm', # finds $VERSION
    'dist' => { COMPRESS => 'gzip', SUFFIX => 'gz'},
);
