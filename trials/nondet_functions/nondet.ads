with Types; use Types;
function Nondet return My_Sub
  with Global => Null,
  Annotate => (ASVAT, Nondet),
  Import => True;
