#!/usr/local/bin/jperl
open(IN, "step1") || die "step1 : $!";

$KANA = "[����-������]";
$KANJI = "[^����-������]";
$ONSETSU = "[������������-�¤�-�����-������][����-������]*";
# ����Ͼ����������󡢲����ǻϤޤäƤϤ����ʤ�

$| = 1;

print "ñ�� -> �ɤ� ��ɽ�������";

sub add_sokuon {
    my ($yomi, $k) = @_;
    my ($org_yomi) = $yomi;
    $yomi =~ s/��$/��/;
    $yomi =~ s/��$/��/;
    $YOMI{$k} .= "!$yomi";
}
sub kata_hira {
    $_[0] =~ tr/��-�������/��-�󤦤���/;
}

while (<IN>) {
    chop;
    print "." if ($n++ % 100 == 0);
    local($yomi, $kanji) = split(/:/);
    if ($kanji =~ /^($KANA*)($KANJI)($KANA*)$/o) {
	my ($header, $k, $trailer) = ($1, $2, $3);
	my $this_yomi;
	if ($yomi =~ /^$header(.*)$trailer$/) {
	    $this_yomi = $1;
	} else {
	    my $kanji2 = $kanji;
	    kata_hira($kanji2);
	    ($header, $k, $trailer) = $kanji2 =~ /^($KANA*)($KANJI)($KANA*)$/o;
	    unless ($yomi =~ /^$header(.*)$trailer$/) {
		warn "�ɤ�($yomi)�ȴ���($kanji)�����פ��Ƥޤ���";
		next;
	    }
	    $this_yomi = $1;
	}
	next if $k eq '';
	$YOMI{$k} .= "!$this_yomi";
	
	&add_sokuon($this_yomi, $k);

	# ���ط�����Ͽ���롣
	$first_yomi = substr($this_yomi, 0, 2);
	$first_yomi =~ tr/����������/����������/;
	$first_yomi =~ tr/����������/����������/;
	$first_yomi =~ tr/�����ĤƤ�/���¤ŤǤ�/;
	$first_yomi =~ tr/�ϤҤդؤ�/�ФӤ֤٤�/;
	$alternative_yomi = $first_yomi . substr($this_yomi, 2);
	$YOMI{$k} .= "!$alternative_yomi";
	&add_sokuon($alternative_yomi);
	
	$first_yomi =~ tr/�ФӤ֤٤�/�ѤԤפڤ�/;
	$alternative_yomi = $first_yomi . substr($this_yomi, 2);
	$YOMI{$k} .= "!$alternative_yomi";
	&add_sokuon($alternative_yomi);
	$first_yomi =~ tr/��/��/;
	$alternative_yomi = $first_yomi . substr($this_yomi, 2);
	$YOMI{$k} .= "!$alternative_yomi";
	&add_sokuon($alternative_yomi);
    }
}
for $kanji (keys %YOMI) {
    %yomis = ();
    for (split(/!/, $YOMI{$kanji})) {
	$yomis{$_} = 1 if ($_ ne "");
    }
    $YOMI{$kanji} = join("!", keys %yomis);
}
delete $YOMI{""};
close IN;

open(OUT, ">yomis.txt");
for $k (sort keys %YOMI) {
    print OUT "$k\t$YOMI{$k}\n";
}
close OUT;

print "..\n";
print "���������";
open(IN, "step1") || die "step1 : $!";
open(OUT, ">pd_kihon.yom") || die "pd_kihon.yom : $!";
open(JUKUJIKU, ">jukujiku.maz") || die "jukujiku.maz : $!";

$i = 0;
while (<IN>) {
    chop;
    print "." if ($n++ % 100 == 0);
    local($yomi, $kanji, $type) = split(/:/);
    next if ($type == 2);
    if (length($kanji) == 2) {
	print OUT "$kanji<$yomi>\n";
    } else {
	$output = &match_itr($yomi, $kanji);
	#print "$kanji : output = $output\n";
	if (!$output) {
	    warn "$kanji($yomi) ���ɤߤ�����";
	    print JUKUJIKU "$yomi $kanji\n";
	} else {
	    print OUT "$output\n";
	}
    }
    #last if ($i >= 3000);
    $i++;
}

print "�ɤߤ�ʬ����ʤ��ϸ�ϡ�jukujiku.maz����Ͽ����ޤ���.\n";

sub match_itr {
    my ($yomi, $kanji) = @_;
    my @kanji = $kanji =~ /\G($KANA+|$KANJI)/go;
    if (join("", @kanji) ne $kanji) {
	warn "�Ѥʻ������äƤ�: $kanji";
	return undef;
    }
    my $pat = '';
    my @unk;
    for (my $i=0; $i<@kanji; $i++) {
	my $k = $kanji[$i];
	if ($k eq '��' && $i > 0) {
	    $k = $kanji[$i-1];
	}
	if ($k =~ /$KANA+/o) {
	    $pat .= "($k)";
	} elsif (defined($YOMI{$k})) {
	    my $p = $YOMI{$k};
	    $p =~ tr/!/|/;
	    $pat .= "($p)";
	} else {
	    push(@unk, $k);
	    $pat .= "((?:$ONSETSU)+?)";
	}
    }
    if (@unk > 1) {
	warn "$kanji---�狼��ʤ�����ʣ��";
	return undef;
    }
    my @ret = $yomi =~ /\A$pat\Z/;
    if (@ret == @kanji) {
	# ����
	my $ret = '';
	for (my $i=0; $i<@kanji; $i++) {
	    $ret .= $kanji[$i];
	    if ($kanji[$i] !~ /$KANA+/o) {
		$ret .= "<$ret[$i]>";
	    }
	}
	$ret;
    } else {
	warn "$yomi !~ $pat";
	undef;
    }
}

sub match_itr_old {
    local($yomi, $kanji) = @_;
    local($output, $kanji_head, $yomi_head, $rest_yomi, $new_output, $orig_kanji_head);
    #print "TRYING-head $yomi, $kanji\n";
    while ($kanji) {
	$kanji_head = substr($kanji, 0, 2);
	$kanji = substr($kanji, 2);
	if ($kanji_head =~ /^[��-������]$/) {
	    $yomi_head = substr($yomi, 0, 2);
	    $yomi = substr($yomi, 2);
	    if ($yomi_head ne $kanji_head) {
		return undef;
	    }
	    $output .= $kanji_head;
	} else {
 	    $orig_kanji_head = $kanji_head;
 	    if ($kanji_head eq "��") {
 		$kanji_head = $last_kanji_head;
 	    }
	    unless (defined $YOMI{$kanji_head}) {
		warn "$kanji_head ���ɤߤ���Ͽ����Ƥޤ���";
		last;
	    }
	    #print "TRYING $kanji_head, $YOMI{$kanji_head}\n";
	    for $kanji_yomi (split(/!/, $YOMI{$kanji_head})) {
		if (index($yomi, $kanji_yomi) == 0) {
		    #local($output);
		    $rest_yomi = substr($yomi, length($kanji_yomi));
		    if ($rest_yomi ne "") {
			$last_kanji_head = $kanji_head;
			
			if ($new_output = &match_itr($rest_yomi, $kanji)) {
			    $output .= "$orig_kanji_head<$kanji_yomi>" . $new_output;
			    return $output;
			}
		    } else {
			$output = "$orig_kanji_head<$kanji_yomi>";
			return $output;
		    }
		}
	    }
	    return undef;
	}
    }
    return $output;
}
