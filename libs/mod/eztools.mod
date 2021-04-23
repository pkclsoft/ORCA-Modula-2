(*$Keep 'EZTools' *)
IMPLEMENTATION MODULE EZTools;
(*
  EZTools

  This unit contains a number of tool related routines that can make life
  much easier on the application, in that they take care of the loading and
  unloading of the various tool sets.

  Copyright 1992, 1993
  EZ-Soft

  Written by: P.C. Easdown
*)

FROM Common IMPORT SCB;
FROM M2Lib IMPORT UserID;
FROM QuickDrawII IMPORT scbColorMode, SetSolidPenPat, SetForeColor,
  SetBackColor, SetPenMode, modeOR;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM ToolLocator IMPORT StartStopRecordPtr, StartupTools, ShutDownTools;

(*$Pascal+*)
PROCEDURE SYSGRAPHTEXTSTARTUP;
PROCEDURE SYSGRAPHTEXTSHUTDOWN;
(*$Pascal-*)

PROCEDURE EZStartDesk(inMasterSCB: SCB): StartStopRecordPtr;
(*
  Similar to the StartDesk routine in ORCA/Pascal, except that it loads almost
  every tool in existance and requires the application to supply the master
  SCB, rather than a resolution value. This is so that the application is able
  to set some of the other bits that have been defined with version 3.1 of 
  quickdraw.  The other major difference, is that the application is required
  to mainatin a pointer that is returned from the function, so that it may 
  pass it to EZEndDesk when shutting down.
*)
VAR
  EZStartStopRecord: StartStopRecordPtr;
BEGIN
  NEW(EZStartStopRecord);
 
  WITH EZStartStopRecord^ DO
    flags := 0;
    videoMode := inMasterSCB;
    numTools := 26;
 
    toolArray[01].toolNumber := ToolLocator;
    toolArray[01].minVersion := 0300H;
    toolArray[02].toolNumber := ADBToolSet;
    toolArray[02].minVersion := 0201H;
    toolArray[03].toolNumber := TextToolSet;
    toolArray[03].minVersion := 0201H;
    toolArray[04].toolNumber := MemoryManager;
    toolArray[04].minVersion := 0300H;
    toolArray[05].toolNumber := SANEToolSet;
    toolArray[05].minVersion := 0202H;
    toolArray[06].toolNumber := ResourceManager;
    toolArray[06].minVersion := 0100H;
    toolArray[07].toolNumber := MiscToolSet;
    toolArray[07].minVersion := 0300H;
    toolArray[08].toolNumber := Scheduler;
    toolArray[08].minVersion := 0200H;
    toolArray[09].toolNumber := SystemLoader;
    toolArray[09].minVersion := 0300H;
    toolArray[10].toolNumber := QuickDrawII;
    toolArray[10].minVersion := 0301H;
    toolArray[11].toolNumber := QuickDrawIIAux;
    toolArray[11].minVersion := 0301H;
    toolArray[12].toolNumber := EventManager;
    toolArray[12].minVersion := 0300H;
    toolArray[13].toolNumber := WindowManager;
    toolArray[13].minVersion := 0301H;
    toolArray[14].toolNumber := ControlManager;
    toolArray[14].minVersion := 0301H;
    toolArray[15].toolNumber := MenuManager;
    toolArray[15].minVersion := 0301H;
    toolArray[16].toolNumber := LineEditToolSet;
    toolArray[16].minVersion := 0301H;
    toolArray[17].toolNumber := DialogManager;
    toolArray[17].minVersion := 0302H;
    toolArray[18].toolNumber := SoundToolSet;
    toolArray[18].minVersion := 0301H;
    toolArray[19].toolNumber := NoteSynthesizer;
    toolArray[19].minVersion := 0104H;
    toolArray[20].toolNumber := StandardFile;
    toolArray[20].minVersion := 0301H;
    toolArray[21].toolNumber := ScrapManager;
    toolArray[21].minVersion := 0300H;
    toolArray[22].toolNumber := DeskManager;
    toolArray[22].minVersion := 0302H;
    toolArray[23].toolNumber := ListManager;
    toolArray[23].minVersion := 0301H;
    toolArray[24].toolNumber := FontManager;
    toolArray[24].minVersion := 0301H;
    toolArray[25].toolNumber := PrintManager;
    toolArray[25].minVersion := 0300H;
    toolArray[26].toolNumber := TextEditToolSet;
    toolArray[26].minVersion := 0101H;
  END;

  SYSGRAPHTEXTSTARTUP;
  EZGraphicsActive := TRUE;
 
  RETURN VAL(StartStopRecordPtr, StartupTools(UserID(), 0, EZStartStopRecord));
END EZStartDesk;

PROCEDURE EZEndDesk(inStartStopRecord: StartStopRecordPtr);
(*
  Will shut down all of the tools started by EZStartDesk.  This procedure must
  be supplied with the startstop record pointer that was returned from 
  EZStartDesk.
*)
BEGIN
  SYSGRAPHTEXTSHUTDOWN;
  EZGraphicsActive := FALSE;
  ShutDownTools(0, inStartStopRecord);
END EZEndDesk;

PROCEDURE EZStartGraph(inMasterSCB: SCB): StartStopRecordPtr;
(*
  Similar to the StartGraph routine in ORCA/Pascal, except that it loads 
  quickdraw auxilliary and the event manager as well.  The application must
  supply the master SCB, rather than a resolution value so that the 
  application is able to set some of the other bits that have been defined
  with version 3.1 of quickdraw.  The other major difference, is that the 
  application is required to mainatin a pointer that is returned from the
  function, so that it may pass it to EZEndGraph when shutting down
*)
CONST
  black       = 0;
  white320    = 15;
  white640    = 3;
VAR
  EZStartStopRecord:  StartStopRecordPtr;
  returnValue:        StartStopRecordPtr;
BEGIN
  NEW(EZStartStopRecord);
 
  WITH EZStartStopRecord^ DO
    flags := 0;
    videoMode := inMasterSCB;
    numTools := 4;
 
    toolArray[01].toolNumber := ToolLocator;
    toolArray[01].minVersion := 0300H;
    toolArray[02].toolNumber := QuickDrawII;
    toolArray[02].minVersion := 0301H;
    toolArray[03].toolNumber := QuickDrawIIAux;
    toolArray[03].minVersion := 0301H;
    toolArray[04].toolNumber := EventManager;
    toolArray[04].minVersion := 0300H;
  END;
 
  returnValue := VAL(StartStopRecordPtr, StartupTools(UserID(), 0, EZStartStopRecord));
 
  IF scbColorMode IN inMasterSCB THEN
    SetSolidPenPat(white640);
    SetForeColor(white640);
  ELSE
    SetSolidPenPat(white320);
    SetForeColor(white320)
  END;

  SetBackColor(black);
  SetPenMode(modeOR);

  SYSGRAPHTEXTSTARTUP;
  EZGraphicsActive := TRUE;

  RETURN returnValue;
END EZStartGraph;

PROCEDURE EZEndGraph(inStartStopRecord: StartStopRecordPtr);
(*
  Will shut down all of the tools started by EZStartGraph.  This procedure 
  must be supplied with the startstop record pointer that was returned from 
  EZStartGraph.
*)
BEGIN
  SYSGRAPHTEXTSHUTDOWN;
  ShutDownTools(0, inStartStopRecord);
  EZGraphicsActive := FALSE;
END EZEndGraph;

BEGIN
  EZGraphicsActive := FALSE;
END EZTools.
