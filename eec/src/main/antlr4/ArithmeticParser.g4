parser grammar ArithmeticParser;

options {
	tokenVocab = ArithmeticLexer;
}

translationUnit: expr | NUMBER;

expr: NUMBER operation NUMBER;

operation: (ADD | SUB | MUL | DIV);