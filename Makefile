all:
	yacc -d restart.y
	lex restart.l
	gcc lex.yy.c y.tab.c -o ex
	 