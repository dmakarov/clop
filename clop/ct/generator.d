import clop.ct.grammar;
import pegged.grammar;

version (CLOP_GENERATE_PARSER)
void main()
{
  asModule ("clop.ct.parser", "parser", clop_rules);
}
