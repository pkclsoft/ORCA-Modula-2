(*$Segment M2FM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2FM; (* Hermann Seiler 19.2.86 / 10.6.86 / 28.4.89 *)

(* Interface to the MC68881 Floating-Point Coprocessor *)
(* or an equivalent Floating-Point Emulator (SANE).    *)

FROM M2DM IMPORT
  MaxInt, StrPtr, ConstValue, Structure, Standard, aAdr, aSize,
  notyp, cardtyp, inttyp, lcardtyp, dbltyp, realtyp, lrltyp;
FROM M2HM IMPORT
  byte, word, long, quad, stackPointer, GetReg, RegAdr,
  Condition, PushAdr, Release, LoadVal, PushVal, LoadAdr,
  WidType, ItemMode, Item, DRegister, Unlock, LoadD,
  SimpleT, RealT, SignedT, SetregMd, SetDregMd, SetstkMd,
  LoadX, Move, MoveBlock, CheckDbltoSingle;
FROM M2LM IMPORT
  PutOp, PutDPReference, PutStackReference, AddBytesToObjectFile;
FROM M2OMF IMPORT
  GenGlobalOp;
FROM M2SM IMPORT
  Mark, Symbol;
FROM SYSTEM IMPORT
  WORD;
FROM W65C816 IMPORT JSL, PEA, PLA, STA, DIRECT, LDA, PHA, STX, DIRINDLONG,
  IMMEDIATE3, LDY, DIRINDBYYLG, IMMEDIATE, STKRELATIVE, EOR, And;

TYPE
  aFloatingOperation =
  (FADDs, FSUBs, FMULs, FDIVs, FNEGs, FABSs, FLOATs, TRUNCs, FEQUs, FGEQs, FGRTs,
   FADDd, FSUBd, FMULd, FDIVd, FNEGd, FABSd, FLOATd, TRUNCd, FEQUd, FGEQd, FGRTd,
   FLONG, FSHORT, FUNKNOWN);

  PROCEDURE ZeroVal(VAR x : Item) : BOOLEAN;
    VAR b : BOOLEAN;
  BEGIN b := FALSE;
    IF x.mode = conMd THEN
      IF    x.typ = realtyp THEN b := x.val.R = FLOAT(0)
      ELSIF x.typ = lrltyp  THEN b := x.val.X = FLOATD(0)
      END;
    END;
    RETURN b
  END ZeroVal;

  PROCEDURE SetfltMd(VAR x: Item; fR: aAdr; ftyp : StrPtr);
  BEGIN
    WITH x DO
      typ := ftyp;
      mode := fltMd;
      stkAdr := fR;
    END;
  END SetfltMd;

  PROCEDURE LoadF(VAR x : Item);
  (*
    OPERATION:
      Load x into a "Floating-Point-Register".
  *)
  VAR
    saveStack:  aAdr;
    reg:        DRegister;
  BEGIN
    WITH x DO
      IF mode < stkMd THEN
	      saveStack := stackPointer;
	      PushAdr(x);

	      IF typ = realtyp THEN (* single real *)
	        GenGlobalOp(JSL, '~LOADREAL', 0, 0, 3);
          AddBytesToObjectFile(4);  
	      ELSE (* double real *)
	        GenGlobalOp(JSL, '~LOADDOUBLE', 0, 0, 3);
          AddBytesToObjectFile(4);  
        END;

        (*
          The above routines leave the extended value on the stack.
        *)
        stackPointer := saveStack + ExtendedSize;
        mode := fltMd;
        stkAdr := stackPointer;
        Release(x);
      ELSIF mode = conMd THEN
	      saveStack := stackPointer;

	      IF typ = realtyp THEN (* single real *)
	        PutDPReference(PEA, val.D1, 3);
	        PutDPReference(PEA, val.D0, 3);
	        GenGlobalOp(JSL, '~LoadRealConstant', 0, 0, 3);
          AddBytesToObjectFile(4);  
	      ELSE (* double real *)
	        PutDPReference(PEA, val.D3, 3);
	        PutDPReference(PEA, val.D2, 3);
	        PutDPReference(PEA, val.D1, 3);
	        PutDPReference(PEA, val.D0, 3);
	        GenGlobalOp(JSL, '~LoadDoubleConstant', 0, 0, 3);
          AddBytesToObjectFile(4);  
        END;

        (*
          The above routines leave the extended value on the stack.
        *)
        stackPointer := saveStack + ExtendedSize;
        mode := fltMd;
        stkAdr := stackPointer;
      ELSIF mode = stkMd THEN
        IF NOT indir THEN
          IF typ = realtyp THEN
            GetReg(word, reg);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg), 2);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg)+2, 2);
            DEC(stackPointer, 4);
          ELSE
            GetReg(quad, reg);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg), 2);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg)+2, 2);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg)+4, 2);
            PutOp(PLA, 1);
            PutDPReference(STA+DIRECT, RegAdr(reg)+6, 2);
            DEC(stackPointer, 8);
          END;

          SetDregMd(x, reg, typ);
        END;

        saveStack := stackPointer;

        PushAdr(x);

        IF typ = realtyp THEN
	        GenGlobalOp(JSL, '~LOADREAL', 0, 0, 3);
          AddBytesToObjectFile(4);  
        ELSE
	        GenGlobalOp(JSL, '~LOADDOUBLE', 0, 0, 3);
          AddBytesToObjectFile(4);  
        END;

        stackPointer := saveStack + ExtendedSize;
        Unlock(reg);
        SetfltMd(x, stackPointer, x.typ)
      ELSIF mode <> fltMd THEN
        Mark(239);
        Release(x);
        mode := fltMd;
        stkAdr := 0;
      END;
    END (*WITH*);
  END LoadF;

  PROCEDURE FMove(VAR x, y : Item);
  (* floating move  x  --->>  y           *)
  (* perform floating type moves :        *)
  (*        memory    to   memory         *)
  (*        register  to   memory         *)
  (*        memory    to   register       *)
  VAR
    saveStack:  aAdr;
    reg:        DRegister;
    indirect:   BOOLEAN;
    z:          Item;
  BEGIN
    (* Assert(y.mode <> conMd); *)

    IF x.typ = realtyp THEN (* single real *)
      IF (x.mode <> fltMd) AND (y.mode <> fltMd) THEN
        (*
          Neither of them are extended values, so do a simple longword move.
        *)
        Move(x,y);
      ELSIF (x.mode <> fltMd) AND (y.mode = fltMd) THEN
        (*
          We are loading x from a REAL value to the stack as an extended value.
        *)
        Release(y);
        y := x;
        LoadF(y);
      ELSIF (x.mode = fltMd) AND (y.mode < stkMd) THEN
        (*
          We are storing the extended value of x as a REAL in y.
        *)
        IF stackPointer - x.stkAdr = 0 THEN
  	      saveStack := stackPointer;
          z := y;
          PushAdr(z);
	        GenGlobalOp(JSL, '~SAVEREAL', 0, 0, 3);
          AddBytesToObjectFile(4);  
          stackPointer := saveStack - ExtendedSize;
        ELSE
          Mark(211);
        END;
      ELSIF (x.mode = fltMd) AND (y.mode = stkMd) THEN
        indirect := y.indir;

        IF NOT indirect THEN
          GetReg(long, reg);
          SetDregMd(y, reg, y.typ);
        END;

        saveStack := stackPointer;

        PushAdr(y);

        GenGlobalOp(JSL, '~SAVEREAL', 0, 0, 3);
        AddBytesToObjectFile(4);  

        stackPointer := saveStack - ExtendedSize;

        IF NOT indirect THEN
          PutDPReference(LDA+DIRECT, RegAdr(reg)+2, 2);
          PutOp(PHA, 1);
          PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
          PutOp(PHA, 1);

          INC(stackPointer, 4);
          Unlock(reg);
          SetstkMd(y, y.typ);
        END;
      ELSE
        Mark(241);
      END;
    ELSE (* double real *)
      IF (x.mode < stkMd) AND (y.mode <= stkMd) THEN
        (*
          A memory to memory move. So do a simple block move.
        *)
        MoveBlock(x,y,SIZE(LONGREAL),FALSE);
      ELSIF (x.mode = fltMd) AND (y.mode < stkMd) THEN
        (*
          Converting an extended value back to a LONGREAL and storing it.
        *)
        IF stackPointer - x.stkAdr = 0 THEN
  	      saveStack := stackPointer;
          z := y;
          PushAdr(z);
          GenGlobalOp(JSL, '~SAVEDOUBLE', 0, 0, 3);
          AddBytesToObjectFile(4);  
          stackPointer := saveStack - ExtendedSize;
        ELSE
          Mark(211);
        END;
      ELSIF ((x.mode < stkMd) OR (x.mode = conMd)) AND
            (y.mode = fltMd) THEN
        (*
          We are loading x from a LONGREAL value to the stack as an extended
          value.
        *)
        Release(y);
        y := x;
        LoadF(y);
      ELSIF x.mode = stkMd THEN
        IF y.mode <= stkMd THEN
          IF x.indir OR x.notpop THEN
            MoveBlock(x, y, SIZE(LONGREAL), FALSE);
          ELSIF (y.mode <> stkMd) OR ((y.mode = stkMd) AND y.indir) THEN
            IF y.mode = DregMd THEN
              PutOp(PLA, 1);
              PutDPReference(STA+DIRECT, RegAdr(y.register), 2);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRECT, RegAdr(y.register)+2, 2);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRECT, RegAdr(y.register)+4, 2);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRECT, RegAdr(y.register)+6, 2);
              DEC(stackPointer, 8);
            ELSE
              GetReg(long, reg);
              z := y;
              LoadAdr(z);
              Unlock(erCPU);
              PutDPReference(STA+DIRECT, RegAdr(reg), 2);
              PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);

              PutOp(PLA, 1);
              PutDPReference(STA+DIRINDLONG, RegAdr(reg), 2);
              PutDPReference(LDY+IMMEDIATE3, 2, 3);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
              PutDPReference(LDY+IMMEDIATE3, 4, 3);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
              PutDPReference(LDY+IMMEDIATE3, 6, 3);
              PutOp(PLA, 1);
              PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
              Unlock(reg);
              DEC(stackPointer, 8);
            END;
          END;
        ELSIF y.mode = fltMd THEN
          GetReg(quad, reg);

          PutOp(PLA, 1);
          PutDPReference(STA+DIRECT, RegAdr(reg), 2);
          PutOp(PLA, 1);
          PutDPReference(STA+DIRECT, RegAdr(reg)+2, 2);
          PutOp(PLA, 1);
          PutDPReference(STA+DIRECT, RegAdr(reg)+4, 2);
          PutOp(PLA, 1);
          PutDPReference(STA+DIRECT, RegAdr(reg)+6, 2);
          DEC(stackPointer, 8);

          SetDregMd(x, reg, x.typ);
          saveStack := stackPointer;
          PushAdr(x);
          GenGlobalOp(JSL, '~LOADDOUBLE', 0, 0, 3);
          AddBytesToObjectFile(4);  
          stackPointer := saveStack + ExtendedSize;
          Unlock(reg);
          SetfltMd(y, stackPointer, lrltyp);
        ELSE
          Mark(241);
        END;
      ELSIF (x.mode = fltMd) AND (y.mode = stkMd) THEN
        z := y;
        indirect := z.indir;

        IF NOT indirect THEN
          GetReg(quad, reg);

          SetDregMd(z, reg, y.typ);
        END;

        saveStack := stackPointer;

        PushAdr(z);

        GenGlobalOp(JSL, '~SAVEDOUBLE', 0, 0, 3);
        AddBytesToObjectFile(4);  

        stackPointer := saveStack - ExtendedSize;

        IF NOT indirect THEN
          PutDPReference(LDA+DIRECT, RegAdr(reg)+6, 2);
          PutOp(PHA, 1);
          PutDPReference(LDA+DIRECT, RegAdr(reg)+4, 2);
          PutOp(PHA, 1);
          PutDPReference(LDA+DIRECT, RegAdr(reg)+2, 2);
          PutOp(PHA, 1);
          PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
          PutOp(PHA, 1);

          INC(stackPointer, 8);
          Unlock(reg);
          SetstkMd(y, y.typ);
        END;
      ELSIF (x.mode = conMd) AND (y.mode <= stkMd) THEN
        IF (y.mode = DregMd) AND
           (y.register < erQuad1) THEN
          Mark(241);
        ELSIF (y.mode = stkMd) AND NOT y.indir THEN
	        PutDPReference(PEA, x.val.D3, 3);
 	        PutDPReference(PEA, x.val.D2, 3);
	        PutDPReference(PEA, x.val.D1, 3);
 	        PutDPReference(PEA, x.val.D0, 3);
          INC(stackPointer, 8);
        ELSE
          GetReg(long, reg);
          z := y;
          LoadAdr(z);
          Unlock(erCPU);
          PutDPReference(STA+DIRECT, RegAdr(reg), 2);
          PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);

	        PutDPReference(LDA+IMMEDIATE, x.val.D0, 3);
          PutDPReference(STA+DIRINDLONG, RegAdr(reg), 2);

          PutDPReference(LDY+IMMEDIATE3, 2, 3);
	        PutDPReference(LDA+IMMEDIATE, x.val.D1, 3);
          PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);

          PutDPReference(LDY+IMMEDIATE3, 4, 3);
	        PutDPReference(LDA+IMMEDIATE, x.val.D2, 3);
          PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);

          PutDPReference(LDY+IMMEDIATE3, 6, 3);
	        PutDPReference(LDA+IMMEDIATE, x.val.D3, 3);
          PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);

          Unlock(reg);
        END;
      ELSE (* illegal modes *)
        Mark(241);
      END;
    END (*double*);
  END FMove;

  PROCEDURE FOp1(op: aFloatingOperation; VAR x : Item);
  (* Interface to the SANE interface in module System *)
  (* for monadic Floating-Point-Operations.           *)
  VAR
    regs:       LONGINT;
    y:          Item;
    rtyp:       StrPtr;
    saveStack:  aAdr;
  BEGIN
    (* Assert(RealT(x)); *)            (* even for FLOATs/FLOATd *)

    WITH x DO
      CASE op OF
        FNEGs, FNEGd, FABSs, FABSd, TRUNCs, TRUNCd, FSHORT, FLONG:
          LoadF(x);

		      CASE op OF
		        FNEGs, FNEGd:
		          PutStackReference(LDA+STKRELATIVE, 9);
              PutDPReference(EOR+IMMEDIATE, 8000H, 3);
		          PutStackReference(STA+STKRELATIVE, 9);
		      | FABSs, FABSd:
		          PutStackReference(LDA+STKRELATIVE, 9);
              PutDPReference(And+IMMEDIATE, 7FFFH, 3);
		          PutStackReference(STA+STKRELATIVE, 9);
		      | TRUNCs:
		          GenGlobalOp(JSL, '~CNVREALINT', 0, 0, 3);
              AddBytesToObjectFile(4);  
							SetregMd(x, inttyp);
							stackPointer := stackPointer - ExtendedSize;
		      | TRUNCd:
		          GenGlobalOp(JSL, '~CNVREALLONG', 0, 0, 3);
              AddBytesToObjectFile(4);  
							SetregMd(x, dbltyp);
							stackPointer := stackPointer - ExtendedSize;
		      | FSHORT, FLONG:
		          (*
		            Don't do anything.  If we have an extended value on the stack, then
		            that is all that is necessary.  When we go to store it, it will be
		            converted then.
		          *)
		      END;
        |
        FLOATs, FLOATd:
          IF x.typ^.size = VAL(aSize, 2) THEN
            IF SignedT(x) THEN
              (*
                We can convert an integer to a real using ~CnvIntReal
              *)
              LoadVal(x, erCPU);
              GenGlobalOp(JSL, '~CNVINTREAL', 0, 0, 3);
              AddBytesToObjectFile(4);  
							stackPointer := stackPointer + ExtendedSize;

              Unlock(erCPU);

							IF op = FLOATs THEN
							  SetfltMd(x, stackPointer, realtyp);
							ELSE
							  SetfltMd(x, stackPointer, lrltyp);
							END;
            ELSE
              (*
                But, a cardinal isn't so easy.  Load it as a longint, and use
                ~M2CnvLE to convert it.
              *)
              LoadX(x, long);
              LoadD(x);
              saveStack := stackPointer;
              PushAdr(x);
              GenGlobalOp(JSL, '~M2CnvLE', 0, 0, 3);
              AddBytesToObjectFile(4);  
							stackPointer := saveStack + ExtendedSize;

							IF op = FLOATs THEN
							  SetfltMd(x, stackPointer, realtyp);
							ELSE
							  SetfltMd(x, stackPointer, lrltyp);
							END;
            END;
          ELSIF x.typ^.size = VAL(aSize, 4) THEN
            LoadD(x);
            saveStack := stackPointer;
            PushAdr(x);
            GenGlobalOp(JSL, '~M2CnvLE', 0, 0, 3);
            AddBytesToObjectFile(4);  
  					stackPointer := saveStack + ExtendedSize;

						IF op = FLOATs THEN
						  SetfltMd(x, stackPointer, realtyp);
						ELSE
						  SetfltMd(x, stackPointer, lrltyp);
						END;
          ELSE
            (*
              We don't know how to convert from byte values to reals.
            *)
            Mark(238);
          END;
      END;
    END (*WITH*);
  END FOp1;

  PROCEDURE FOp2(op: aFloatingOperation; VAR x, y : Item);
  (* Interface to the SANE interface in module System *)
  (* for dyadic Floating-Point-Operations.            *)
  VAR
    rtyp:         StrPtr;
    orderMatters: BOOLEAN;
    reg:          DRegister;
    z, w:         Item;
  BEGIN
    (* Assert(RealT(x)); *)

    CASE op OF
      (* define resulting type *)
      FADDs, FSUBs, FMULs, FDIVs:
        rtyp := realtyp;
	    |
      FADDd, FSUBd, FMULd, FDIVd:
        rtyp := lrltyp;
	    ELSE
	      rtyp := notyp;
    END;

    CASE op OF
      (* define resulting type *)
      FADDs, FADDd, FMULs, FMULd, FEQUs, FEQUd:
        orderMatters := FALSE;
	    |
      FSUBs, FSUBd, FDIVs, FDIVd, FGEQs, FGEQd, FGRTs, FGRTd:
        orderMatters := TRUE;
    END;

    IF ((y.mode = stkMd) AND (NOT y.indir) AND (NOT y.notpop)) AND
       ((x.mode = stkMd) AND (NOT x.indir) AND (NOT x.notpop)) THEN
      (*
        Both items are on the stack, and are not extended numbers.
      *)
      IF orderMatters THEN
        (*
          x must come before y, so ensure that they are in that order.
        *)
        IF x.stkAdr > y.stkAdr THEN
          (*
            x is currently after y, so pull them both off, and swap them
            around.
          *)
          GetReg(quad, reg);
          SetDregMd(z, reg, x.typ);
          FMove(x, z);

          GetReg(quad, reg);
          SetDregMd(w, reg, y.typ);
          FMove(y, w);
          LoadF(z);
          LoadF(w);
        ELSE
          (*
            s is before y, but we must pull y off before converting x to an
            extended number.  Only then can we load y back onto the stack as
            an extended number.
          *)
          GetReg(quad, reg);
          SetDregMd(z, reg, y.typ);
          FMove(y, z);
          LoadF(x);
          LoadF(z);
        END;
      ELSE
        (*
          Order doesn't matter, but we have to convert them both to extended
          numbers, and in doing so, ensure that we do them in the right order.
        *)
        IF x.stkAdr > y.stkAdr THEN
          (*
            x is after y on the stack, so pull it off, convert y, and then
            reload x as an extended.
          *)
          GetReg(quad, reg);
          SetDregMd(z, reg, y.typ);
          FMove(y, z);
          LoadF(x);
          LoadF(z);
        ELSE
          (*
            y is after x on the stack, so pull it off, convert x, and then
            reload y as an extended.
          *)
          GetReg(quad, reg);
          SetDregMd(z, reg, x.typ);
          FMove(x, z);
          LoadF(y);
          LoadF(z);
        END;
      END;
    ELSIF ((y.mode = stkMd) AND (NOT y.indir) AND (NOT y.notpop)) OR
          (y.mode = fltMd) THEN
      (*
        Note that we assume at this point that both items ARE NOT ON THE
        STACK already, unless in the form of extended numbers.
      *)
      IF orderMatters THEN
        GetReg(quad, reg);
        SetDregMd(z, reg, y.typ);
        FMove(y, z);
        LoadF(x);
        LoadF(z);
      ELSE
        LoadF(y);
        LoadF(x);
      END;
    ELSE
      LoadF(x);
      LoadF(y);
    END;

    CASE op OF
      FADDs, FADDd:
				GenGlobalOp(JSL, '~ADDE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - ExtendedSize;
        SetfltMd(x, stackPointer, rtyp);
			|
			FSUBs, FSUBd:
				GenGlobalOp(JSL, '~SUBE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - ExtendedSize;
        SetfltMd(x, stackPointer, rtyp);
			|
			FMULs, FMULd:
				GenGlobalOp(JSL, '~MULE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - ExtendedSize;
        SetfltMd(x, stackPointer, rtyp);
			|
			FDIVs, FDIVd:
				GenGlobalOp(JSL, '~DIVE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - ExtendedSize;
        SetfltMd(x, stackPointer, rtyp);
      |
      FEQUs, FEQUd:
				GenGlobalOp(JSL, '~EQUE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - (ExtendedSize * 2);
      |
      FGEQs, FGEQd:
				GenGlobalOp(JSL, '~GEQE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - (ExtendedSize * 2);
      |
      FGRTs, FGRTd:
				GenGlobalOp(JSL, '~GRTE', 0, 0, 3);
        AddBytesToObjectFile(4);  
				stackPointer := stackPointer - (ExtendedSize * 2);
    END;
  END FOp2;

  PROCEDURE FMonad(op : FMonadic; VAR x : Item);
  (* interface to the SANE monadic operators :  *)
  VAR 
	  cd: aFloatingOperation;
		y:  Item;
  BEGIN
    cd := FUNKNOWN; (* indicates NO FOp1-call *)
		
    CASE op OF
	      Abs :           IF x.typ = lrltyp THEN 
												  cd := FABSd;
											  ELSE
												  cd := FABSs;
												END;
	    | Float :         cd := FLOATs;
	    | FloatD :        cd := FLOATd;
	    | Long :          cd := FLONG;
	    | Short :         cd := FSHORT;
	    | Trunc :         IF x.typ <> realtyp THEN
			                    Mark(241);
											  END;
												
	                      FOp1(TRUNCs, x);
	    | TruncD :        IF x.typ <> lrltyp THEN 
			                    Mark(241);
												END;
												
	                      FOp1(TRUNCd,x);
	    | NonStand :      IF x.typ = lrltyp THEN 
												  cd := FNEGd;
												ELSE
												  cd := FNEGs;
												END;
	    ELSE              Mark(200);
    END (*CASE*);
		
    IF cd <> FUNKNOWN THEN
      FOp1(cd, x);
    END;
  END FMonad;

  PROCEDURE FDyad(op : FDyadic; VAR x, y : Item);
  (* interface to the SANE dyadic operators :  *)
  VAR cd: aFloatingOperation;
  BEGIN
    cd := FUNKNOWN; (* indicates NO FOp2-call *)

		CASE op OF
	      times :         cd := FMULs;
	    | slash :         cd := FDIVs;
			
			                  IF ZeroVal(y) THEN 
												  Mark(205);
												END;
	    | rem   :         Mark(200);
	    | plus  :         cd := FADDs;
	    | minus :         cd := FSUBs;
			| eql:            cd := FEQUs; (* equal                  - ~EquE     *)
			| neq:            cd := FEQUs; (* not equal              - NOT ~EquE *)
			| lss:            cd := FGEQs; (* less than              - NOT ~GeqE *)
			| leq:            cd := FGRTs; (* less than or equal     - NOT ~GrtE *)
			| gtr:            cd := FGRTs; (* greater                - ~GrtE     *)
			| geq:            cd := FGEQs; (* greater than or equal  - ~GeqE     *)
	    ELSE              Mark(200);
    END (*CASE*);
		
    IF cd <> FUNKNOWN THEN
      IF x.typ = lrltyp THEN 
			  INC(cd, ORD(FADDd)); (* take double precision *)
			END;
			
      FOp2(cd, x, y);
    END;
  END FDyad;

END M2FM.
