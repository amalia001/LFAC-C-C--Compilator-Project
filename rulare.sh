flex lim.l &&
bison -d proiect.y &&
gcc lex.yy.c proiect.tab.c &&
./a.out init.txt