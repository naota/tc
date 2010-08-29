#!/usr/local/bin/jperl
open(IN, "step1") || die "step1 : $!";

$KANA = "[ぁあ-ヶー―]";
$KANJI = "[^ぁあ-ヶー―]";
$ONSETSU = "[あいうえおか-ぢつ-もやゆよ-ろわゐゑを][ぁあ-ヶー―]*";
# 音節は小さい字、ん、音引で始まってはいけない

$| = 1;

print "単漢 -> 読み の表を作成中";

sub add_sokuon {
    my ($yomi, $k) = @_;
    my ($org_yomi) = $yomi;
    $yomi =~ s/つ$/っ/;
    $yomi =~ s/く$/っ/;
    $YOMI{$k} .= "!$yomi";
}
sub kata_hira {
    $_[0] =~ tr/ァ-ンヴヵヶ/ぁ-んうかけ/;
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
		warn "読み($yomi)と漢字($kanji)が一致してません。";
		next;
	    }
	    $this_yomi = $1;
	}
	next if $k eq '';
	$YOMI{$k} .= "!$this_yomi";
	
	&add_sokuon($this_yomi, $k);

	# 音便形も登録する。
	$first_yomi = substr($this_yomi, 0, 2);
	$first_yomi =~ tr/かきくけこ/がぎぐげご/;
	$first_yomi =~ tr/さしすせそ/ざじずぜぞ/;
	$first_yomi =~ tr/たちつてと/だぢづでど/;
	$first_yomi =~ tr/はひふへほ/ばびぶべぼ/;
	$alternative_yomi = $first_yomi . substr($this_yomi, 2);
	$YOMI{$k} .= "!$alternative_yomi";
	&add_sokuon($alternative_yomi);
	
	$first_yomi =~ tr/ばびぶべぼ/ぱぴぷぺぽ/;
	$alternative_yomi = $first_yomi . substr($this_yomi, 2);
	$YOMI{$k} .= "!$alternative_yomi";
	&add_sokuon($alternative_yomi);
	$first_yomi =~ tr/ぢ/じ/;
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
print "辞書作成中";
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
	    warn "$kanji($yomi) の読みが不明";
	    print JUKUJIKU "$yomi $kanji\n";
	} else {
	    print OUT "$output\n";
	}
    }
    #last if ($i >= 3000);
    $i++;
}

print "読みの分からない熟語は，jukujiku.mazに登録されました.\n";

sub match_itr {
    my ($yomi, $kanji) = @_;
    my @kanji = $kanji =~ /\G($KANA+|$KANJI)/go;
    if (join("", @kanji) ne $kanji) {
	warn "変な字が入ってる: $kanji";
	return undef;
    }
    my $pat = '';
    my @unk;
    for (my $i=0; $i<@kanji; $i++) {
	my $k = $kanji[$i];
	if ($k eq '々' && $i > 0) {
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
	warn "$kanji---わからない字が複数";
	return undef;
    }
    my @ret = $yomi =~ /\A$pat\Z/;
    if (@ret == @kanji) {
	# 成功
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
	if ($kanji_head =~ /^[あ-ヶー―]$/) {
	    $yomi_head = substr($yomi, 0, 2);
	    $yomi = substr($yomi, 2);
	    if ($yomi_head ne $kanji_head) {
		return undef;
	    }
	    $output .= $kanji_head;
	} else {
 	    $orig_kanji_head = $kanji_head;
 	    if ($kanji_head eq "々") {
 		$kanji_head = $last_kanji_head;
 	    }
	    unless (defined $YOMI{$kanji_head}) {
		warn "$kanji_head の読みが登録されてません";
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
