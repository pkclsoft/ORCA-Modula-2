IMPLEMENTATION MODULE TestLocals2;

  MODULE NESTED2;

  IMPORT B;
  EXPORT INCB, INCBTWICE, INCBTHRICE;

  PROCEDURE INCB;
  BEGIN
    INC(B);
  END INCB;

    MODULE NESTED3;

    IMPORT B, INCB;
    EXPORT INCBTWICE, INCBTHRICE;

    PROCEDURE INCBTWICE;
    BEGIN
      INCB;  (* use outer procedure this time *)
      INC(B); (* do it locally *)
    END INCBTWICE;

    PROCEDURE INCBTHRICE;
    BEGIN
      INCB;  (* use outer procedure *)
      INCBTWICE; (* use local procedure *)
    END INCBTHRICE;

    PROCEDURE LOCAL3;
    BEGIN
    END LOCAL3;

    BEGIN
      B := 4;
      INCBTHRICE; (* sets B to 7 *)
      INCBTWICE; (* sets B to 9 *)
    END NESTED3;

  PROCEDURE LOCAL2;
  BEGIN
  END LOCAL2;

  BEGIN
    INCB; (* sets B to 10 *)
  END NESTED2;

BEGIN
  INCBTWICE; (* sets B to 12 *)
  INCBTHRICE; (* sets B to 15 *)
END TestLocals2.
