(*%F tops*)
(*$Segment ConsoleIO*)
(*%E *)
IMPLEMENTATION MODULE ConsoleIO;

FROM ASCII IMPORT
  nul;
FROM GSOSInterface IMPORT
  DStatus, dStatusDCB, DControl, devReadWriteDCB, DRead, DWrite,
  GSOSInString, GetDevNumber, getDevNumDCB, openDCB, Open;
FROM Storage IMPORT
  ALLOCATE, DEALLOCATE;
FROM SYSTEM IMPORT
  ADR, ADDRESS, BYTE;

CONST
  rawMode     = 8000H;
  noWaitMode  = 8000H;

  (*
    Standard I/O prefixes.
  *)
  StdIn     = 10;
  StdOut    = 11;
  StdError  = 12;

VAR
  consoleDeviceNumber:  CARDINAL;

PROCEDURE DisposePort(VAR port: pTextPortRec);
(*
  OPERATION:
    Will dispose of any memory used by the port.  If successful, it sets
    "port" to NIL.

    Use this when you are finished with a port record that was created using
    NewPort;
*)
BEGIN
  DISPOSE(port);
END DisposePort;

PROCEDURE GetTextPort(VAR consoleTextPort: aTextPortRec);
(*
  OPERATION:
    Copies the current text ports details to the supplied text port record.
*)
VAR
  statusParms:  dStatusDCB;
BEGIN
  WITH statusParms DO
    pCount := 5;
    devNum := consoleDeviceNumber;
    statusCode := 8000H;
    statusList := ADR(consoleTextPort);
    requestCount := 16;
  END;

  DStatus(statusParms);
END GetTextPort;

PROCEDURE GotoXY(x, y: CARDINAL);
(*
  OPERATION:
    Places the cursor at the specified coordinates.

    Warning: No check is made to ensure that the co-ordinates fit within the
    current text port. The console driver should do that.
*)
BEGIN
  WriteChar(scGotoXY);
  WriteChar(CHR(x+32));
  WriteChar(CHR(y+32));
END GotoXY;

PROCEDURE NewPort(top:    CARDINAL;
                  left:   CARDINAL;
                  length: CARDINAL;
                  width:  CARDINAL): pTextPortRec;
(*
  OPERATION:
    Creates a new text port with the specified dimensions, and returns a 
    pointer to the appropriate text port record for manipulation.

    If the port record is no longer required, then use DisposePort to free
    any memory used by the port.

    Note: The new port inherits all of the attributes of the current port
    except the dimensions.
*)
VAR
  port: pTextPortRec;

BEGIN
  NEW(port);

  IF port <> NIL THEN
    GetTextPort(port^);

    WITH port^ DO
      ch := VAL(BYTE, left);
      cv := VAL(BYTE, top);
      windLeft := VAL(BYTE, left);
      windTop := VAL(BYTE, top);
      windRight := VAL(BYTE, (left + width - 1));
      windBottom := VAL(BYTE, (top + length - 1));
      windWidth := VAL(BYTE, width);
      windLength := VAL(BYTE, length);
    END;

    SetTextPort(port);

    WriteChar(scClearAndHome);
  END;

  RETURN port;
END NewPort;

PROCEDURE ReadChar(VAR ch: CHAR; VAR OK: BOOLEAN);
(*
  OPERATION:
    Attempt to read a character from the console.  If no character is read,
    then OK returns FALSE.

    Note: No cursor is displayed.
*)
VAR
  readParms:  devReadWriteDCB;
  buff:       CHAR;
BEGIN
  WITH readParms DO
    pCount := 6;
    devNum := consoleDeviceNumber;
    buffer := ADR(ch);
    requestCount := 1;
    blockSize := 0;
  END;

  DRead(readParms);

  WITH readParms DO
    IF transferCount = VAL(LONGINT, 1) THEN
      OK := TRUE;
    ELSE
      OK := FALSE;
      ch := nul;
    END;
  END;
END ReadChar;

PROCEDURE SetReadMode(mode: CARDINAL);
(*
  OPERATION:
    Set the current read mode to that specified.
*)
VAR
  controlParms:  dStatusDCB;
BEGIN
  WITH controlParms DO
    pCount := 5;
    devNum := consoleDeviceNumber;
    statusCode := 8003H;
    statusList := ADR(mode);
    requestCount := 2;
  END;

  DControl(controlParms);
END SetReadMode;

PROCEDURE SetTextPort(consoleTextPort: pTextPortRec);
(*
  OPERATION:
    Change the current text port to the dimensions and status as specified
    by "consoleTextPort".
*)
VAR
  flags: BITSET;
BEGIN
  WITH consoleTextPort^ DO
    WriteChar(scSetPortSize);
    WriteChar(CHR(VAL(CARDINAL, windLeft) + 32));
    WriteChar(CHR(VAL(CARDINAL, windTop) + 32));
    WriteChar(CHR(VAL(CARDINAL, windRight) + 32));
    WriteChar(CHR(VAL(CARDINAL, windBottom) + 32));

    flags := {};

    IF consWrap = VAL(BYTE, 80H) THEN
      INCL(flags, 10);
    END;

    IF consAdvance = VAL(BYTE, 80H) THEN
      INCL(flags, 8);
    END;

    IF consLF = VAL(BYTE, 80H) THEN
      INCL(flags, 9);
    END;

    IF consScroll = VAL(BYTE, 80H) THEN
      INCL(flags, 11);
    END;

    IF consDLE = VAL(BYTE, 80H) THEN
      INCL(flags, 12);
    END;

    (*
      This effectively sets the high byte to $15 which is just happens to
      be the control code for scSetCursorMove.
    *)
    flags := flags + {0, 2, 4};

    WriteNChars(ADR(flags), 2);

    IF consVideo = VAL(BYTE, 80H) THEN
      WriteChar(scSetNormal);
    ELSE
      WriteChar(scSetInverse);
    END;

    IF consMouse = VAL(BYTE, 80H) THEN
      WriteChar(scEnableMouse);
    ELSE
      WriteChar(scDisableMouse);
    END;

    GotoXY(VAL(CARDINAL, ch) - VAL(CARDINAL, windLeft),
           VAL(CARDINAL, cv) - VAL(CARDINAL, windTop));
  END;
END SetTextPort;

PROCEDURE WriteChar(buff: CHAR);
(*
  OPERATION:
    Write one character to the current text port at the cursor position.
*)
VAR
  writeParms: devReadWriteDCB;
BEGIN
  WITH writeParms DO
    pCount := 6;
    devNum := consoleDeviceNumber;
    buffer := ADR(buff);
    requestCount := 1;
    blockSize := 0;
  END;

  DWrite(writeParms);
END WriteChar;

PROCEDURE WriteNChars(buff: ADDRESS; count: CARDINAL);
(*
  OPERATION:
    Write "count" characters from "buff" to the current text port.

    Note: Buff may contain control sequences.
*)
VAR
  writeParms: devReadWriteDCB;
BEGIN
  WITH writeParms DO
    pCount := 6;
    devNum := consoleDeviceNumber;
    buffer := buff;
    requestCount := count;
    blockSize := 0;
  END;

  DWrite(writeParms);
END WriteNChars;

VAR
  deviceName:   GSOSInString;
  devNumParms:  getDevNumDCB;
  controlParms: dStatusDCB;
  noWait:       CARDINAL;
  openParms:    openDCB;
BEGIN
  (*
    Establish the device number of the console device.
  *)
  WITH deviceName DO
    length := 8;
    text := ".Console";
  END;

  WITH devNumParms DO
    pCount := 2;
    devName := ADR(deviceName);
  END;

  GetDevNumber(devNumParms);

  consoleDeviceNumber := devNumParms.devNum;

  (*
    Now set it to No-Wait mode, since this best suits the purposes of the
    standard Modula-2 terminal routines.
  *)

  noWait := noWaitMode;

  WITH controlParms DO
    pCount := 5;
    devNum := consoleDeviceNumber;
    statusCode := 0004H;
    statusList := ADR(noWait);
    requestCount := 2;
  END;

  DControl(controlParms);

  (*
    In addition to being in No-Wait mode, we will put it into Raw Mode.
  *)

  SetReadMode(rawMode);

  WITH openParms DO
    pCount := 2;
    pathName := ADR(deviceName);
  END;

  Open(openParms);
END ConsoleIO.
