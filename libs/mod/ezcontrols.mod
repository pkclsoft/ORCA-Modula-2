IMPLEMENTATION MODULE EZControls;

FROM Common IMPORT Ref;
FROM ControlManager IMPORT CtlHandle, cSimpleButtonControl, simpBRound,
  inactiveHilite, HiliteControl, simpleButton, noHilite, GetCtlID,
  SendEventToCtl, FindTargetCtl, cLineEditControl, FindControl,
  cTextEditControl, GetCtlHandleFromID, GetCtlValue, SetCtlValue,
  DrawControls;
FROM EventManager IMPORT EventModifier, EventModifierBit, appleKey, keyPad,
  keyDownEvt, autoKeyEvt, updateEvt, EventMask, EventBit;
FROM LineEdit IMPORT LERecHandle, LECut, LEPaste, LECopy, LEDelete, LEKey;
FROM ListManager IMPORT NextMember2, SelectMember2, SortList2, NewList2,
  MemberDrawProc, CompareProc;
FROM M2Lib IMPORT LoWORD, HighWORD, ToolError;
FROM MemoryManager IMPORT Handle;
FROM MenuManager IMPORT HiliteMenu;
FROM QuickDrawII IMPORT GrafPort, GrafPortPtr, GetPort, SetPort;
FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
FROM TextEdit IMPORT TECut, TEPaste, TECopy, TEClear;
FROM WindowManager IMPORT TaskRec, FrontWindow, TaskMaster, TaskMasterDA,
  wInControl, wInSpecial, wInMenuBar, TaskMask, TaskMaskBit, BeginUpdate,
  GetContentDraw, ContentDrawProc, EndUpdate;

(*
  These routines are used internally by EZDialogEvent and EZAnyEvent.
*)

PROCEDURE EZHandleKeyEvent(EventRec: TaskRec): LONGINT;
CONST
  CutKey          = "X";  (* X key       *)
  PasteKey        = "V";  (* V key       *)
  CopyKey         = "C";  (* C key       *)
  DeleteKey       = 157C; (* Delete key  *)
  ReturnKey       = 13;   (* Return key  *)
  controlOrOption = EventModifier{emControlDown, emOptionDown};
  noModifiers     = EventModifier{};
  funnyKeys       = EventModifier{emAppleDown, emOptionDown, emControlDown};

TYPE
  aKeySet = SET OF [0..127];

VAR
  Key             : CARDINAL;
  theControl      : CtlHandle;
  itemHit         : LONGINT;
  EventReceived   : BOOLEAN;
 
  PROCEDURE EZProcessReturnKey() : LONGINT;
  VAR
    thisControl : CtlHandle;
    thisPort    : pEZGrafPort;
    itemHit     : LONGINT;
    temp:         BITSET;
  BEGIN
    thisPort := VAL(pEZGrafPort, FrontWindow());
    itemHit := 0;

    IF thisPort <> NIL THEN
      thisControl := thisPort^.wControl;
     
      WHILE (thisControl <> NIL) AND
            (itemHit = 0) DO
        IF VAL(LONGINT, thisControl^^.ctlProc) = cSimpleButtonControl THEN
          temp := VAL(BITSET, thisControl^^.ctlFlag) * VAL(BITSET, 0083H);
          IF (VAL(CARDINAL, temp) = simpBRound) AND
             (thisControl^^.ctlHilite <> VAL(BYTE, inactiveHilite))    THEN
            HiliteControl(simpleButton, thisControl);
            HiliteControl(noHilite, thisControl);
            itemHit := GetCtlID(thisControl);
          END;
        END;
     
        thisControl := thisControl^^.ctlNext;
      END;
    END;
     
    RETURN itemHit;
  END EZProcessReturnKey;
 
  PROCEDURE EZHandleTextEditKey(Key:        CARDINAL;
                                theControl: CtlHandle;
                                EventRec:   TaskRec) : LONGINT;
  VAR
    SendEventJunk       : BOOLEAN;
  BEGIN
    IF (Key IN aKeySet{3,4,6,8,10,11,13,21,22,24,25,32..127}) OR
       ((EventRec.wmModifiers * controlOrOption) <> noModifiers) THEN
      IF (EventRec.wmModifiers * appleKey) = appleKey THEN
        CASE CAP(CHR(Key)) OF
          CutKey      : TECut(NIL);
        | PasteKey    : TEPaste(NIL);
        | CopyKey     : TECopy(NIL);
        | DeleteKey   : TEClear(NIL);
        ELSE
          SendEventJunk := SendEventToCtl(TRUE, NIL, ADR(EventRec));
        END;
      ELSE
        SendEventJunk := SendEventToCtl(TRUE, NIL, ADR(EventRec));
      END;
    END;
    
    RETURN GetCtlID(theControl);
  END EZHandleTextEditKey;

  PROCEDURE ClipboardKey(Key: CARDINAL): BOOLEAN;
  BEGIN
    RETURN (Key IN aKeySet{67,86,88,99,118,120}) AND
           ((EventRec.wmModifiers * appleKey) = appleKey);
  END ClipboardKey;
    
  PROCEDURE EZHandleLineEditKey(Key:        CARDINAL;
                                theControl: CtlHandle;
                                EventRec:   TaskRec) : LONGINT;
  VAR
    leControl   : LERecHandle;
    leType      : LONGINT;

    PROCEDURE validKey(Key: CARDINAL) : BOOLEAN;
    BEGIN
      IF ClipboardKey(Key)
          OR
         ((leType = lineEditAllKeys) AND
          ((Key IN aKeySet{6,8,21,24,25,32..127}) OR
           ((EventRec.wmModifiers * controlOrOption) <> noModifiers)))
          OR
         ((leType = lineEditAlphaNumeric) AND
          (Key IN aKeySet{6,8,21,24,25,48..57,65..90,97..122,127}))
          OR
         ((leType = lineEditAlphabetic) AND
          (Key IN aKeySet{6,8,21,24,25,65..90,97..122,127}))
          OR
         ((leType = lineEditNumeric) AND
          (Key IN aKeySet{6,8,21,24,25,48..57,127}))
          OR
         ((leType = lineEditPathname) AND
          (Key IN aKeySet{6,8,21,24,25,46,58,48..57,65..90,97..122,127})) THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END validKey;

  BEGIN
    leControl := VAL(LERecHandle, theControl^^.ctlData);
    leType := theControl^^.ctlRefCon;

    IF validKey(Key) THEN
      IF (EventRec.wmModifiers * appleKey) = appleKey THEN
        CASE CAP(CHR(Key)) OF
          CutKey    : LECut(leControl);
        | PasteKey  : LEPaste(leControl);
        | CopyKey   : LECopy(leControl);
        | DeleteKey : LEDelete(leControl);
        ELSE
          LEKey(Key, EventRec.wmModifiers, leControl);
        END;
      ELSE
        LEKey(Key, EventRec.wmModifiers, leControl);
      END;
    END;
     
    RETURN GetCtlID(theControl);
  END EZHandleLineEditKey;

  PROCEDURE GiveToSystem;
  BEGIN
    EventReceived := SendEventToCtl(FALSE, NIL, ADR(EventRec));

    IF EventReceived THEN
      theControl := VAL(CtlHandle, EventRec.wmTaskData2);
      itemHit := GetCtlID(theControl);
    ELSE
      itemHit := 0;
    END;
  END GiveToSystem;
   
BEGIN
  Key := LoWORD(EventRec.wmMessage);

  (*
    IF the key has a modifier, is not one of the cliboard keys THEN
      give the keystroke to "SendEventToCtl".
    ELSIF the target control is either a line or text edit THEN
      use our routines
    ELSE
      give the key to "SendEventToCtl".
    END;
  *)
  IF (((EventRec.wmModifiers * funnyKeys) <> noModifiers) AND
      NOT ClipboardKey(Key))
     OR
     (Key IN aKeySet{13, 27}) THEN
    GiveToSystem;
  ELSE
    (*
      Determine which control the cut/paste is for
    *)
    theControl := FindTargetCtl();

    IF theControl <> NIL THEN (* If there is a ctl capable of taking this operation *)
      IF VAL(LONGINT, theControl^^.ctlProc) = cLineEditControl THEN
        itemHit := EZHandleLineEditKey(Key, theControl, EventRec);
      ELSIF VAL(LONGINT, theControl^^.ctlProc) = cTextEditControl THEN
        itemHit := EZHandleTextEditKey(Key, theControl, EventRec);
      ELSE
        GiveToSystem;
      END;
    ELSE
      GiveToSystem;
    END;
  END;

  RETURN itemHit;
END EZHandleKeyEvent;

PROCEDURE EZHandleLineEditMenu(theControl : CtlHandle;
                               EventRec   : TaskRec) : LONGINT;
CONST
  EditUndo            = 250;
  EditCut             = 251;
  EditCopy            = 252;
  EditPaste           = 253;
  EditClear           = 254;
VAR
  MenuNum     : CARDINAL;
  leControl   : LERecHandle;
BEGIN
  (* We only actually do this if the target control is an Edit  *)
  (* line control.                                              *)
 
  MenuNum := LoWORD(VAL(LONGINT, EventRec.wmTaskData));
 
  leControl := VAL(LERecHandle, theControl^^.ctlData);
 
  CASE MenuNum OF
    EditCut         : LECut(leControl);
  | EditCopy        : LECopy(leControl);
  | EditPaste       : LEPaste(leControl);
  | EditClear       : LEDelete(leControl);
  ELSE
  END;
 
  RETURN GetCtlID(theControl);
END EZHandleLineEditMenu;

PROCEDURE HandleTextEditMenu(theControl : CtlHandle;
                             EventRec   : TaskRec) : LONGINT;
CONST
  EditUndo            = 250;
  EditCut             = 251;
  EditCopy            = 252;
  EditPaste           = 253;
  EditClear           = 254;
VAR
  MenuNum     : CARDINAL;
BEGIN
  MenuNum := LoWORD(VAL(LONGINT, EventRec.wmTaskData));
 
  CASE MenuNum OF
    EditCut         : TECut(NIL);
  | EditCopy        : TECopy(NIL);
  | EditPaste       : TEPaste(NIL);
  | EditClear       : TEClear(NIL);
  ELSE
  END;
 
  RETURN GetCtlID(theControl);
END HandleTextEditMenu;

PROCEDURE EZHandleControl(EventRec : TaskRec) : LONGINT;
VAR
  ControlHit      : CtlHandle;
  FindControlJunk : CARDINAL;
  junk:             LONGINT;
BEGIN
  FindControlJunk := FindControl(ControlHit,
                                 EventRec.wmWhere.x,
                                 EventRec.wmWhere.y,
                                 VAL(GrafPortPtr, EventRec.wmTaskData));

  IF (FindControlJunk <> 0) THEN
    junk:= GetCtlID(ControlHit);

    IF ToolError() <> 0 THEN
      RETURN 0;
    ELSE
      RETURN junk;
    END;
  ELSE
    RETURN 0;
  END;
END EZHandleControl;

PROCEDURE HandleEditMenu(EventRec : TaskRec) : LONGINT;
VAR
  theControl: CtlHandle;
  junk:       LONGINT;
BEGIN
  (* Determine which control the cut/paste is for *)
  theControl := FindTargetCtl();
 
  IF theControl <> NIL THEN (* If there is a control capable of taking this operation *)
    IF VAL(LONGINT, theControl^^.ctlProc) = cLineEditControl THEN
      junk := EZHandleLineEditMenu(theControl, EventRec);
    ELSIF VAL(LONGINT, theControl^^.ctlProc) = cTextEditControl THEN
      junk := HandleTextEditMenu(theControl, EventRec);
    END;
  ELSE
    junk := 0;
  END;
 
  (* Revert the menu back to normal *)
  HiliteMenu(FALSE, HighWORD(VAL(LONGINT, EventRec.wmTaskData)));

  RETURN junk;
END HandleEditMenu;

PROCEDURE EZDialogEvent(VAR MultiClick: CARDINAL): LONGINT;
(*
  EZDialogEvent This function calls TaskMaster to handle any system events,
  ~~~~~~~~~~~~~ returns the item ID of the control that was hit. If an event
  occurs outside a control, THEN the item ID returned it zero. EZDialogEvent
  also takes care of things which TaskMaster does not, such as the cut,
  and paste facilities for a lineEdit control.

  MultiClick  - Upon returning, this parameter holds the value returned by
                Taskmaster in the ClickCount field of the task record, thus
                enabling the application to determine whether a double or
                triple click has occured.
*)
CONST
  eventMask = EventMask{ebDown, ebUp, ebKeyDown, ebAutoKey, ebUpdate, 
                         ebDeskAcc};
VAR
  Event               : CARDINAL;
  LastEvent           : TaskRec;
  itemHit             : LONGINT;
BEGIN
  LastEvent.wmTaskMask := TaskMask{tmUpdate, tmFindW, tmSpecial, tmCRedraw,
                                    tmInfo, tmContentControls, tmControlMenu,
                                    tmMultiClick, tmIdleEvents};
          (* %0000 0000 0001 1111 1011 0000 0000 0110 *)
 
  Event := TaskMaster(eventMask, LastEvent);  (* Get an event                   *)
          (* %0000 0100 0110 1110 *)
 
  CASE Event OF
    keyDownEvt,
    autoKeyEvt  :   itemHit := EZHandleKeyEvent(LastEvent);
  | wInControl  :   itemHit := EZHandleControl(LastEvent);
  | wInSpecial  :   itemHit := HandleEditMenu(LastEvent);
  | updateEvt   :   itemHit := Event;
                    EZUpdateWindow(VAL(GrafPortPtr, LastEvent.wmMessage));
  ELSE
    itemHit := 0;
  END;
 
  MultiClick := LastEvent.wmClickCount;
 
  RETURN itemHit;           (* Return the itemID of the control    *)
END EZDialogEvent;

PROCEDURE EZAnyEvent(VAR AnyEventRecord:  TaskRec;
                     VAR EventHandled:    BOOLEAN;
                         CheckMenus:      BOOLEAN): LONGINT;
(*
  EZAnyEvent This function calls TaskMaster to handle any system events, and
  ~~~~~~~~~~ returns one of several types of values, as follows:

  ControlID  - If an event has occured inside a control, THEN the ID of that
               control is returned.
  MenuItemID - If a menu item has been selected, THEN the ID of the selected
               menu is returned.
  Event      - If neither a control or a menu has been selected, THEN the
               function returns the raw event information in the same format
               as TaskMaster.

  Parameters are as follows:

  AnyEventRecord - An extended event record in which the event information
                   is returned.
  CheckMenus     - A BOOLEAN flag allowing the application to lock out the
                   use of menus.  True will allow menu selection.  False
                   will not.
*)
CONST
  eventMask = EventMask{ebDown, ebUp, ebKeyDown, ebAutoKey, ebUpdate, 
                         ebActivate, ebSwitch, ebDeskAcc, ebDriver};
VAR
  Event               : CARDINAL;
  itemHit             : LONGINT;

BEGIN
  IF CheckMenus THEN
    AnyEventRecord.wmTaskMask := TaskMask{tmMenuKey, tmUpdate, tmFindW,
        tmMenuSel, tmOpenDA, tmSysClick, tmDragW, tmContent, tmClose, tmZoom,
        tmGrow, tmScroll, tmSpecial, tmCRedraw, tmInactive, tmInfo, 
        tmContentControls, tmControlMenu, tmMultiClick, tmIdleEvents};
          (* %0000 0000 0001 1111 1111 1111 1111 1111 *)
  ELSE
    AnyEventRecord.wmTaskMask := TaskMask{tmUpdate, tmFindW,
        tmOpenDA, tmSysClick, tmDragW, tmContent, tmClose, tmZoom,
        tmGrow, tmScroll, tmSpecial, tmCRedraw, tmInactive, tmInfo, 
        tmContentControls, tmControlMenu, tmMultiClick, tmIdleEvents};
          (* %0000 0000 0001 1101 1111 1111 1111 0110 *)
  END;

  Event := TaskMaster(eventMask, AnyEventRecord);  (* Get an event *)
          (* %0000 1111 0110 1110 *)
 
  EventHandled := TRUE;

  CASE Event OF
    keyDownEvt,
    autoKeyEvt  :   itemHit := EZHandleKeyEvent(AnyEventRecord);
  | wInControl  :   itemHit := EZHandleControl(AnyEventRecord);
  | wInSpecial  :   itemHit := HandleEditMenu(AnyEventRecord);
  | wInMenuBar  :   itemHit := LoWORD(VAL(LONGINT, AnyEventRecord.wmTaskData));
  | updateEvt   :   itemHit := Event;
                    EZUpdateWindow(VAL(GrafPortPtr, AnyEventRecord.wmMessage));
                    EventHandled := FALSE;
  ELSE
    itemHit := Event;
    EventHandled := FALSE;
  END;
 
  RETURN itemHit;
END EZAnyEvent;

PROCEDURE EZDAEvent(VAR AnyEventRecord: TaskRec;
                    VAR EventHandled:   BOOLEAN;
                        CheckMenus:     BOOLEAN): LONGINT;
(*
  EZDAEvent  This function calls TaskMaster to handle any system events, and
  ~~~~~~~~~  returns one of several types of values, as follows:

             Essentially, this function is identical to EZAnyEvent, except
             for the fact that this function calls TaskMasterDA and not the
             normal TaskMaster entry point.  As such, this function is
             designed specifically for use with NDA's.

  ControlID  - If an event has occured inside a control, THEN the ID of that
               control is returned.
  MenuItemID - If a menu item has been selected, THEN the ID of the selected
               menu is returned.
  Event      - If neither a control or a menu has been selected, THEN the
               function returns the raw event information in the same format
               as TaskMaster.

  Parameters are as follows:

  AnyEventRecord - An extended event record in which the event information
                   is returned.
  CheckMenus     - A BOOLEAN flag allowing the application to lock out the
                   use of menus.  True will allow menu selection.  False
                   will not.
*)
VAR
  Event               : CARDINAL;
  itemHit             : LONGINT;
BEGIN
  HALT;
(*  Event := TaskMasterDA(EventMask{}, AnyEventRecord);   (* Get an event *)
            (* %0000 1111 0110 1110 *)
 *)
  EventHandled := TRUE;

  CASE Event OF
    keyDownEvt,
    autoKeyEvt  : itemHit := EZHandleKeyEvent(AnyEventRecord);
  | wInControl  : itemHit := EZHandleControl(AnyEventRecord);
  | wInSpecial  : itemHit := HandleEditMenu(AnyEventRecord);
  | wInMenuBar  : itemHit := LoWORD(VAL(LONGINT, AnyEventRecord.wmTaskData));
  ELSE
    itemHit := Event;
    EventHandled := FALSE;
  END;
 
  RETURN itemHit;
END EZDAEvent;

PROCEDURE EZNextMember(inItem:  CARDINAL;
                       inID:    LONGINT): CARDINAL;
(*
  EZNextMember  Returns the next member number in the extended list contol
  ~~~~~~~~~~~~  specified by inID.

  inItem   -  Is the first member in the list at which to start searching
              for another selected item.
  inID     -  Is the item ID of the extended list control.
*)
VAR
  workCtlHandle   : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  RETURN NextMember2(inItem, VAL(Handle, workCtlHandle));
END EZNextMember;

PROCEDURE EZSelectMember(inItem:  CARDINAL;
                         inID:    LONGINT);
(*
  EZSelectMember  Selects the specified member in the extended list contol
  ~~~~~~~~~~~~~~  specified by inID.

  inItem   -  Is the number of the member to be selected.
  inID     -  Is the item ID of the extended list control.
*)
VAR
  workCtlHandle   : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  SelectMember2(inItem, VAL(Handle, workCtlHandle));
END EZSelectMember;

PROCEDURE EZSortList(inID:  LONGINT);
(*
  EZSortList  Will sort the specified list using the default sort routine.
  ~~~~~~~~~~
  inID     - The Item ID of the list control.
*)
VAR
  workCtlHandle   : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  SortList2(CompareProc(NIL), VAL(Handle, workCtlHandle));
END EZSortList;

PROCEDURE EZNewList(inDrawProc: MemberDrawProc;
                    inStart:    CARDINAL;
                    inList:     Ref;
                    inSize:     CARDINAL;
                    inSelect:   CARDINAL;
                    inID:       LONGINT);
(*
  EZNewList  Will place register the new list contents with the list manager
  ~~~~~~~~~  and sort it using the default sort routine, before finally
             selecting the specified member.

  inDrawProc - A pointer to a custom draw member routine.
  inStart    - This is the first member to be displayed in the list window.
  inList     - The new list structure to be passed to NewList2.
  inSize     - The number of members in the list.
  inSelect   - The number of the member to be selected.
  inID       - The Item ID of the list control.
*)
VAR
  workCtlHandle   : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);

  NewList2(inDrawProc,
           inStart,
           inList,
           0,
           inSize,
           VAL(Handle, workCtlHandle));

  EZSortList(inID);

  EZSelectMember(inSelect, inID);
END EZNewList;

PROCEDURE RadioButtonOn(inID: LONGINT): BOOLEAN;
(*
  RadioButtonOn  Returns a BOOLEAN value indicating whether the radio button
  ~~~~~~~~~~~~~  specified by inID is turned on. A TRUE result indicates ON.
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  RETURN VAL(BOOLEAN, GetCtlValue(workCtlHandle));
END RadioButtonOn;

PROCEDURE CheckBoxOn(inID: LONGINT): BOOLEAN;
(*
  CheckBoxOn  Returns a BOOLEAN value indicating whether the check box
  ~~~~~~~~~~  specified by inID is turned on. A TRUE result indicates ON.
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  RETURN VAL(BOOLEAN, GetCtlValue(workCtlHandle));
END CheckBoxOn;

PROCEDURE SetRadioButton(inValue: BOOLEAN;
                         inID:    LONGINT);
(*
  SetRadioButton  Sets the Radio Button specified by inID to either an ON or
  ~~~~~~~~~~~~~~  OFF state.

  inValue      -  This parameter may be either TRUE or FALSE.  FALSE will turn
                  the button off.  TRUE will turn the button on.
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  SetCtlValue(VAL(CARDINAL, inValue), workCtlHandle);
END SetRadioButton;

PROCEDURE SetCheckBox(inValue:  BOOLEAN;
                      inID:     LONGINT);
(*
  SetCheckBox  Sets the Check Box specified by inID to either an ON or OFF
  ~~~~~~~~~~~  state.

  inValue      -  This parameter may be either TRUE or FALSE.  FALSE will turn
                  the button off.  TRUE will turn the button on.
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  SetCtlValue(VAL(CARDINAL, inValue), workCtlHandle);
END SetCheckBox;

PROCEDURE Deactivate(inID: LONGINT);
(*
  Deactivate  Will deactivate the specified extended control.
  ~~~~~~~~~~
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  HiliteControl(inactiveHilite, workCtlHandle);
END Deactivate;

PROCEDURE Activate(inID: LONGINT);
(*
  Activate  Will deactivate the specified extended control.
  ~~~~~~~~
*)
VAR
  workCtlHandle : CtlHandle;
BEGIN
  workCtlHandle := GetCtlHandleFromID(NIL, inID);
  HiliteControl(noHilite, workCtlHandle);
END Activate;

PROCEDURE EZDrawControls;
(*
  EZDrawControls  This routine simply makes to DrawControls, passing the
  ~~~~~~~~~~~~~~  current port to it.  It may be used when creating a window
                  with nothing but controls in it, for which you need a
                  refresh routine.
*)
BEGIN
  DrawControls(GetPort());
END EZDrawControls;

PROCEDURE EZUpdateWindow(WindowPtr: GrafPortPtr);
(*
  EZUpdateWindow  This procedure is an assembly routine that obtains the
  ~~~~~~~~~~~~~~  address of the specified windows content drawing procedure
  so that it may be called, to effect the update of the specified window.
  normal dialog.

  WindowPtr    - This is the pointer to the window or grafport to be updated
*)
VAR
  DrawContent: ContentDrawProc;
BEGIN
  SetPort(WindowPtr);
  BeginUpdate(WindowPtr);
  DrawContent := GetContentDraw(WindowPtr);

  IF DrawContent <> VAL(ContentDrawProc, NIL) THEN
    DrawContent;
  END;

  EndUpdate(WindowPtr);
END EZUpdateWindow;

END EZControls.
