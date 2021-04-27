IMPLEMENTATION MODULE Bug006;

FROM InOut IMPORT WriteCard, WriteString, WriteLn;

VAR
  p:  documentPtr;
  a:  document;
BEGIN
  WriteString('document = ');
  WriteCard(SIZE(a), 4);
  WriteString(' bytes');
  WriteLn;

  WriteString('documentPtr = ');
  WriteCard(SIZE(p), 4);
  WriteString(' bytes');
  WriteLn;

  WriteString('documentPtr^ = ');
  WriteCard(SIZE(p^), 4);
  WriteString(' bytes');
  WriteLn;
END Bug006.

