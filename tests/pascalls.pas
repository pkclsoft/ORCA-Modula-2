{$keep 'pascalls'}
unit pascalls;

interface

const
  lowarr      = 0;
  smlhigharr  = 1;
  lrghigharr  = 5;

type
  smlarrtyp = array [lowarr..smlhigharr] of integer;
  lrgarrtyp = array [lowarr..lrghigharr] of integer;

  smlrectyp = record
                a:  integer;
                b:  boolean;
              end;

  lrgrectyp = record
                a:  integer;
                b:  boolean;
                c:  longint;
              end;

procedure nonvars(b:  byte;
                  c:  char;
                  i:  integer;
                  bo: boolean;
                  l:  longint;
                  r:  real;
                  db: double);

procedure vars(VAR b:  byte;
               VAR c:  char;
               VAR i:  integer;
               VAR bo: boolean;
               VAR l:  longint;
               VAR r:  real;
               VAR db: double);

procedure smlnonvararr(arr: smlarrtyp);

procedure smlvararr(VAR arr: smlarrtyp);

procedure lrgnonvararr(arr: lrgarrtyp);

procedure lrgvararr(VAR arr: lrgarrtyp);

procedure smlnonvarrec(rec: smlrectyp);

procedure smlvarrec(VAR rec: smlrectyp);

procedure lrgnonvarrec(rec: lrgrectyp);

procedure lrgvarrec(VAR rec: lrgrectyp);

function incbyte(b: byte): byte;

function incchar(c: char): char;

function incint(i: integer): integer;

function inclint(li: longint): longint;

function notbool(bo: boolean): boolean;

function add10toreal(r: real): real;

function add102tolrl(lr: double): double;

implementation

procedure nonvars;
begin
  writeln('nonvars');
  writeln;
  writeln('b  = <', b, '>');
  writeln('c  = <', c, '>');
  writeln('i  = <', i, '>');
  writeln('bo = <', bo, '>');
  writeln('l  = <', l, '>');
  writeln('r  = <', r, '>');
  writeln('db = <', db, '>');
end;

procedure vars;
begin
  writeln('vars');
  writeln;
  writeln('b  = <', b, '>');
  writeln('c  = <', c, '>');
  writeln('i  = <', i, '>');
  writeln('bo = <', bo, '>');
  writeln('l  = <', l, '>');
  writeln('r  = <', r, '>');
  writeln('db = <', db, '>');
end;

procedure smlnonvararr;
var
  i: integer;
begin
  writeln('smlnonvararr');
  writeln;

  for i := lowarr to smlhigharr do begin
    writeln('arr[',i:1,'] = <', arr[i], '>');
  end;
end;

procedure smlvararr;
var
  i: integer;
begin
  writeln('smlvararr');
  writeln;

  for i := lowarr to smlhigharr do begin
    writeln('arr[',i:1,'] = <', arr[i], '>');
  end;
end;

procedure lrgnonvararr;
var
  i: integer;
begin
  writeln('lrgnonvararr');
  writeln;

  for i := lowarr to lrghigharr do begin
    writeln('arr[',i:1,'] = <', arr[i], '>');
  end;
end;

procedure lrgvararr;
var
  i: integer;
begin
  writeln('lrgvararr');
  writeln;

  for i := lowarr to lrghigharr do begin
    writeln('arr[',i:1,'] = <', arr[i], '>');
  end;
end;

procedure smlnonvarrec;
begin
  writeln('smlnonvarrec');
  writeln;

  with rec do begin
    writeln('a = <', a, '>');
    writeln('b = <', b, '>');
  end;
end;

procedure smlvarrec;
begin
  writeln('smlvarrec');
  writeln;

  with rec do begin
    writeln('a = <', a, '>');
    writeln('b = <', b, '>');
  end;
end;

procedure lrgnonvarrec;
begin
  writeln('lrgnonvarrec');
  writeln;

  with rec do begin
    writeln('a = <', a, '>');
    writeln('b = <', b, '>');
    writeln('c = <', c, '>');
  end;
end;

procedure lrgvarrec;
begin
  writeln('lrgvarrec');
  writeln;

  with rec do begin
    writeln('a = <', a, '>');
    writeln('b = <', b, '>');
    writeln('c = <', c, '>');
  end;
end;

function incbyte;
begin
  incbyte := SUCC(b);
end;

function incchar;
begin
  incchar := SUCC(c);
end;

function incint;
begin
  incint := succ(i);
end;

function inclint;
begin
  inclint := li + 1;
end;

function notbool;
begin
  notbool := NOT bo;
end;

function add10toreal;
begin
  add10toreal := r + 10.0;
end;

function add102tolrl;
begin
  add102tolrl := lr + 102.354;
end;

end.
