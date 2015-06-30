output: lex.yy.c y.tab.c
	gcc y.tab.c -o output

y.tab.c: B10015049.y
	bison -dy B10015049.y

lex.yy.c: B10015049.l
	lex B10015049.l

clean:
	rm -f *.o lex.yy.c y.tab.c y.tab.h output