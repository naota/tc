#!/usr/local/bin/perl5
use Term::Cap;
use Term::Complete;

sub goto_line {
    my $n = shift;
    $term->Tgoto('cm', 7, $n, STDOUT);
}
sub display_line {
    my $n = shift;
    my $path = $FILES[$n]->{name};
    $path = $1 if ($path =~ /(.*)\.dat/);
    $path .= " " x (5 - (length $path));
    my $state_name = $STATE_NAMES[$FILES[$n]->{state}];
    $term->Tgoto('cm', 0, $n, STDOUT);
    print "$path [$state_name] $FILES[$n]->{letters}\n";
    goto_line $n;
}
sub restore_term {
    goto_line $line_max+6;
    system("stty -cbreak echo");
}
sub signal_handler {
    restore_term;
    exit 1;
}
@STATE_NAMES = qw/× △ ◎/;

#  unless (-e "./combime" || -e "./reduce") {
#      print STDERR "You haven't compiled the tools yet. Wait a sec.\n";
#      system("make combine");
#      system("make reduce");
#  }

$term = Tgetent Term::Cap {OSPEED => 9600};
bless $term, Term::Cap;
$term->Tputs('cl', 1, STDOUT);

$n = 0;

for (sort { substr($a,1) <=> substr($b,1); }  <t*.dat>) {
    $FILES[$n] = {};
    $FILES[$n]->{name} = $_;

    my @ALL_LETTERS;
    open(IN, $_) || die "$_ : $!";
    while (<IN>) {
	push @ALL_LETTERS, split;
    }
    my $letters;
    for (0..15) {
	$letters .= "," if ($_ != 0);
	$letters .= $ALL_LETTERS[$_ * $#ALL_LETTERS / 15];
    }
    $FILES[$n]->{letters} = $letters;

    $n++;
}
if (open(STATE, ".state")) {
    while (<STATE>) {
	die ".state : $. : ??\n" unless (/^(t\d+.dat)\s*(\d)$/);
	#print "$1 : $2\n";
	for (0..$n-1) {
	    $FILES[$_]->{state} = $2 if ($FILES[$_]->{name} eq $1);
	}
    }
    close STATE;
}
#exit 1;

select STDIN; $| = 1; select STDOUT; $| = 1;
system("stty cbreak -echo");

for (0..$n-1) {
    display_line $_;
}
$term->Tgoto('cm', 0, $n+2, STDOUT);
print 'n : 下，p : 上, " " : 切換, e : 終了, q : 中断', "\n";
print '◎ : 確実に覚えた文字群', "\n";
print '△ : 一部覚えた文字群', "\n";
print '× : 全く覚えていない文字群', "\n";

$line_max = $n;
$line = 0;
goto_line $line;

$SIG{INT} = $SIG{QUIT} = $SIG{TERM} = 'signal_handler';

for (;;) {
    $ch = getc STDIN;
    if ($ch eq 'n') {
	if ($line < $line_max-1) {
	    goto_line ++$line;
	} else {
	    print "\007";
	}
    } elsif ($ch eq 'p') {
	if ($line > 0) {
	    goto_line --$line;
	} else {
	    print "\007";
	}
    } elsif ($ch eq ' ') {
	my $state = $FILES[$line]->{state};
	$state = ($state + 1) % ($#STATE_NAMES + 1);
	$FILES[$line]->{state} = $state;
	display_line $line;
    } elsif ($ch eq 'q') {
	goto_line $line_max;
	print '本当に中断しますか(y/n)?';
	$ch = getc STDIN;
	if ($ch eq 'y' || $ch eq 'Y') {
	    restore_term;
	    exit 1; # for make
	}
    } elsif ($ch eq 'e') {
	last;
    }
}

restore_term;

open OUT, ">.state";
for (0 .. $line_max-1) {
    print OUT "$FILES[$_]->{name} $FILES[$_]->{state}\n";
}
close OUT;

for (0 .. $line_max-1) {
    if ($FILES[$_]->{state} == 1) {
	$uncertain .= "$FILES[$_]->{name} ";
    } elsif ($FILES[$_]->{state} == 2) {
	$certain .= "$FILES[$_]->{name} ";
    }
}
print("cat /dev/null $uncertain >uncertain\n");
system("cat /dev/null $uncertain >uncertain");
print("cat /dev/null $certain >certain\n");
system("cat /dev/null $certain >certain");

#system("make");
exit 0;
