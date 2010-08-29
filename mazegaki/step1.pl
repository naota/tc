#!/usr/local/bin/jperl

$| = 1;

sub process {
    open(IN, $_[0]) || die "$_[0] : $!";
    while (<IN>) {
	chop;
	print "." if ($n++ % 100 == 0);
	if (($yomi, $kanji, $type) = /^(\S+)\s+(\S+)\s+(\S+)/) {
	    next if ($kanji =~ /^[あ-ヶー]+$/);
	    next if ($kanji =~ /[ァ-ヶー０-９Ａ-ｚ0-9]/);

	    # TYPE == 1 : 動詞語幹
	    # TYPE == 0 : 他
	    
	    # 動詞，形容詞は終止形に直す。
	    if ($type =~ /(.)行.*段/) {
		local($col) = $1;
		$col =~ tr/アカサタマラワガザダバ/うくすつむるうぐずづぶ/;
		$kanji .= "―";
		$yomi .= "―";
		$type = "1";
	    } elsif (index($type, "一段")>=0) {
		$kanji .= "―";
		$yomi .= "―";
	    } elsif ($type eq "形容詞") {
		$kanji .= "―";
		$yomi .= "―";
	    } elsif ($type eq '0' || $type eq '2') {
		;
	    } else {
		$type = "0";
	    }
	    print OUT "$yomi:$kanji:$type\n";
	} else {
	    warn "ignore $_\n";
	}
    }
}

open(OUT, ">step1") || die "step1 : $!";
for $file ('tcode.u', 'tankan.u', 'kihon.u', 'chimei.u', 'computer.u', 'jinmei.u', 'koyuu.u') {
    &process($file);
}
print "\n";
