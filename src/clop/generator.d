import clop.grammar;
import pegged.grammar;

version (CLOP_GENERATE_PARSER)
void main()
{
  asModule ("clop.parser", "parser", clop_rules);
}
