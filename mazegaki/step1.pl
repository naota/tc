#!/usr/local/bin/jperl

$| = 1;

sub process {
    open(IN, $_[0]) || die "$_[0] : $!";
    while (<IN>) {
	chop;
	print "." if ($n++ % 100 == 0);
	if (($yomi, $kanji, $type) = /^(\S+)\s+(\S+)\s+(\S+)/) {
	    next if ($kanji =~ /^[��-����]+$/);
	    next if ($kanji =~ /[��-������-����-��0-9]/);

	    # TYPE == 1 : ư��촴
	    # TYPE == 0 : ¾
	    
	    # ư�졤���ƻ�Ͻ��߷���ľ����
	    if ($type =~ /(.)��.*��/) {
		local($col) = $1;
		$col =~ tr/���������ޥ�塞������/�������Ĥ�뤦�����Ť�/;
		$kanji .= "��";
		$yomi .= "��";
		$type = "1";
	    } elsif (index($type, "����")>=0) {
		$kanji .= "��";
		$yomi .= "��";
	    } elsif ($type eq "���ƻ�") {
		$kanji .= "��";
		$yomi .= "��";
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
