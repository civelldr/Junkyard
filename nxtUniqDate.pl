#!/usr/bin/perl

@mm = qw(
01
02
03
04
05
06
07
08
09
10
11
12);

@dd = qw(
01
02
03
04
05
06
07
08
09
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31);


for ($y = 2015; $y <= 4444; $y++) {
	foreach $m (@mm)  {
		foreach $d (@dd) {
			$str = "$y$m$d";
			@a = split //, $str;
			my %seen;
			my @unique = grep { not $seen{$_} ++ } @a;
			if (scalar(@unique) == 8) { print @unique , "\n"; exit; }
		}
	}
}
