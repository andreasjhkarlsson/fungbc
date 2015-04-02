../../gbdk-n/bin/gbdk-n-compile.sh test.c
../../gbdk-n/bin/gbdk-n-assemble.sh fgbc.s
../../gbdk-n/bin/gbdk-n-link.sh test.rel fgbc.rel -o test.ihx
../../gbdk-n/bin/gbdk-n-make-rom.sh test.ihx test.gb
