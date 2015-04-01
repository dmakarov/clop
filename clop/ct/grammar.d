module clop.ct.grammar;

public import pegged.peg;
import pegged.grammar;

mixin (grammar(mixin ("`" ~ import("grammar.in") ~ "`")));
