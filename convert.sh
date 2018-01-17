sed '
  s/^>//
  t
  s/^ *$//
  t
  s/^/-- /
 ' in.lhs > out.hs
