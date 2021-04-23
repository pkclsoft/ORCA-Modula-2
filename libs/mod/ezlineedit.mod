IMPLEMENTATION MODULE EZLineEdit;

FROM Common IMPORT TextArrayPtr;
FROM ControlManager IMPORT CtlHandle, GetCtlHandleFromID;
FROM LineEdit IMPORT LERecHandle, LESetText, LEGetTextHand, LEGetTextLen,
  LESetSelect;
FROM QuickDrawII IMPORT Rect;
FROM Strings IMPORT Length, Copy;
FROM SYSTEM IMPORT ADR;
FROM WindowManager IMPORT InvalRect;

PROCEDURE setLineEditText(EditText : ARRAY OF CHAR;
                          SelectIt : BOOLEAN;
                          inID     : LONGINT);
(*
  setlineEditText  This procedure will insert the specified text into an
  ~~~~~~~~~~~~~~~  extended lineEdit control.

  EditText     - A pascal string of up to 255 characters that is to be
                 inserted into the control.
  SelectIt     - A boolean flag, indicating whether the routine is to select
                 the text after inserting it.
  inID         - An extended control ID, used to specify the control into
                 which the text is to be inserted.
*)
VAR
  theControl  : CtlHandle;   (* Will point to the extended control.         *)
  EditLCtl    : LERecHandle; (* Will point to the actual line edit control. *)
                             (* We use this for the actual LE calls.        *)
  editCtlRect : Rect;        (* Will contain the control rect for InvalRect *)
BEGIN
  (* Extended line edit control handles cannot be passed direct to LE calls, *)
  (* so it is necessary to get the actual line edit handle from within the   *)
  (* extended control record.  Basically, an edit line is not a control, but *)
  (* can be used as part of one by the control manager.                      *)
 
  theControl := GetCtlHandleFromID(NIL, inID);
 
  IF theControl <> NIL THEN
    EditLCtl := VAL(LERecHandle, theControl^^.ctlData);

    (*
      Set the text of the edit line.
    *)
    LESetText(ADR(EditText), Length(EditText), EditLCtl);
 
    (*
      If told to, then select the entire string.
    *)
    IF SelectIt THEN
      LESetSelect(0, Length(EditText), EditLCtl);
    END;

    editCtlRect := theControl^^.ctlRect;
 
    InvalRect(editCtlRect); (* Invalidate the edit line control rectangle  *)
                            (* so that it will be redrawn.                 *)
  END;
END setLineEditText;

PROCEDURE getLineEditText(    inID         : LONGINT;
                          VAR TextValue    : ARRAY OF CHAR);
(*
  getlineEditText  This procedure obtains the current value of an extended
  ~~~~~~~~~~~~~~~  lineEdit control.

  inID         - An extended control ID, used to specify the control from
                 which the text is to be obtained.
  TextValue    - A pascal string into which the lineEdit value is to be
                 placed.
*)
TYPE
  TextArrayHandle  = POINTER TO TextArrayPtr;
VAR
  TextHandle  : TextArrayHandle;
  TextLength  : CARDINAL;
  theControl  : CtlHandle;   (* Will point to the extended control.         *)
  EditLCtl    : LERecHandle; (* Will point to the actual line edit control. *)
                              (* We use this for the actual LE calls.        *)
BEGIN
  (* Extended line edit control handles cannot be passed direct to LE calls, *)
  (* so it is necessary to get the actual line edit handle from within the   *)
  (* extended control record.  Basically, an edit line is not a control, but *)
  (* can be used as part of one by the control manager.                      *)
 
  theControl := GetCtlHandleFromID(NIL, inID);
 
  IF theControl <> NIL THEN
    EditLCtl := VAL(LERecHandle, theControl^^.ctlData);
 
    (*
      Get a handle to the text.
    *)
    TextHandle := VAL(TextArrayHandle, LEGetTextHand(EditLCtl));

    (*
      Get its length.
    *)
    TextLength := LEGetTextLen(EditLCtl);

    Copy(TextHandle^^, 0, TextLength, TextValue);
  ELSE
    TextValue := '';
  END;
END getLineEditText;

END EZLineEdit.

