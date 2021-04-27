(*$RangeCheck+*)
MODULE Goof;
VAR
  color: [0..3];
BEGIN
  color := 2;
  color := (color * 4) MOD 4;
  color := color + 6;
END Goof.
