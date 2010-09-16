unit WCCUtil;

interface

{$I WCC.inc}

uses Classes, SysUtils, Windows, jpeg,
  {$IFDEF DIRECTX}
  Direct3D9, D3DX9,
  {$ENDIF}
  ADODB, ADOINT, DB,
  Graphics,
  JclDebug,
  SvcMgr,
  WideStrings,
  Forms
  ;

const
  CRLF = #13+#10;

function IsDebugged: Boolean;

{ Inline ifs }
function iif_str(const eval : boolean; const s1, s2 : string):string;
function iif_chr(const eval : boolean; const s1, s2 : Char):Char;
function iif_int(const eval : boolean; const i1, i2 : integer):integer;
function iif_dbl(const eval : boolean; const d1, d2 : double):double;
function iif_sng(const eval : boolean; const d1, d2 : single):single;
function iif_var(const eval : boolean; const v1, v2 : variant):variant;

{ Additional Inc }
function IncDbl(var Value, Amount : Double):Double;

{ Message boxes}
function ConfirmYN(const Msg : string):boolean;
function ConfirmTF(const Msg : string):boolean;
function WarnYN(const Msg : string):boolean;
function WarnYNFmt(const Msg : string; Args : array of const):boolean;
procedure ErrorMsg(const Msg : string);
procedure ErrorMsgFmt(const Msg : string; Args : array of const);
procedure WarnMsg(const Msg : string);
procedure WarnMsgFmt(const Msg : string; Args : array of const);
procedure InfoMsg(const Msg : string);
procedure InfoMsgFmt(const Msg : string; Args : array of const);

{ Registry read Helpers }
function RegReadString(const Key, Value : string):string;
function RegReadInteger(const Key, Value : string):integer;
function RegReadBoolean(const Key, Value : string):Boolean;

{ Registry write helpers}
procedure RegWriteString(const Key, Value, Data : string; Force : boolean = False);
procedure RegWriteInteger(const Key, Value : string; const Data : integer; Force : boolean = False);
procedure RegWriteBoolean(const Key, Value : string; const Data : Boolean; Force : boolean = False);

{ DirectX helpers}
{$IFDEF DIRECTX}
procedure TranslateD3DColor(const Color : TD3DColor; var R, G, B : integer);
function ColorToD3DColor(ATColor : TColor):TD3DColor;
function D3DColorToColor(AD3DColor : TD3DColor):TColor;
{$ENDIF}

{ Interface Helpers }
function RefInterface(const Intf : IUnknown): Pointer;
procedure ReleaseInterface(P : Pointer);

{ String pointer helpers }
function RefString(const s : string):Pointer;
procedure ReleaseString(P : Pointer);

{ Misc }
function UpperChar(const C: Char): Char;
procedure CheckVarInit(ObjClass : TObject);
procedure CheckDBID(const ID : integer);
procedure CheckStrID(const strID : string);
procedure WCCTrace(const Msg : string);
procedure WCCTraceStrings(sList : TStrings);

//procedure AddStringViaStream(sList : TStrings; s : string); overload;
//procedure AddStringViaStream(sList : TWideStrings; s : string);

procedure GetDisplayMetrics(var CX, CY : integer);
function NormalizePath(const Path : string): string;
procedure WCCShowWindow(AForm : TForm);

{ String }
function UpperFirst(const s : string): string;
function UnderscoreToSpace(const s : string): string;

function ExtractFileNameURL(URL : string):string;

//EVENTLOG_SUCCESS           0x0000 	Information event
//EVENTLOG_AUDIT_FAILURE     0x0010 	Failure Audit event
//EVENTLOG_AUDIT_SUCCESS     0x0008 	Success Audit event
//EVENTLOG_ERROR_TYPE        0x0001 	Error event
//EVENTLOG_INFORMATION_TYPE  0x0004 	Information event
//EVENTLOG_WARNING_TYPE      0x0002 	Warning event

procedure WCCLogMessage(Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);

{ Windows API Helpers }
function WinGetTempPath:string;
function WinGetTempFile:string; overload;
function WinGetTempFile(Prefix : string):string; overload;
function GetMyDocsDir:string;
function WinGetSystemDirectory:string;
function WinGetCommonAppDataDir:string;
function WinGetAppDataDir:string;
function WinGetComputerName:string;

{ Database }
function SilentExecSQL(ADOQuery : TADOQuery):integer;
function StrToBool(const s : string):boolean;
function BoolToStr(const b : boolean):string;
procedure SetADODateTimeParameter(P : TParameter; DT : TDateTime);
function NullStr(const s : string):string;
function NVL(value : Variant):boolean;

function CreateAccessDatabase(FileName: string): string;

procedure ScaleBitmap(ABitmap : TBitmap; Scale : Double; Destination : string); overload;
procedure ScaleBitmap(ABitmap : TBitmap; const AWidth, AHeight : integer); overload;
procedure ScaleJPEG(AJPEG : TJPEGImage; const AHeight, AWidth : integer; Destination : string);
procedure DrawGradient(StartColor, EndColor : TColor; TargetCanvas : TCanvas; TargetRect : TRect);

function DoubleIsZero(d1 : double):boolean;
function AreDblEqual(d1, d2 : double):boolean;

procedure ShellExecuteEasy(const FileName : string);
function GetShortName(sLongName: string): string;


type
  TDelimParser = class
  private
    FDelim : Char;
    FSource : string;
    FKeys, FValues : TStringList;
    function GetElements(index: string): string;
    procedure Decompose;
  public
    constructor Create(const s : string; const delim : char);
    destructor Destroy; override;
    property Elements[index : string]:string read GetElements;
  end;

implementation

uses Dialogs, Controls, Registry, COMObj, Variants, ShlObj,
  ShellAPI, SHFolder
//  {$IFDEF DELPHI9}
//  ,jcl_rip
//  {$ELSE}
  ,JclStrings
//  {$ENDIF}
  ;

var
  FEventLogger : TEventLogger = nil;

function GetShortName(sLongName: string): string;
var
  sShortName    : string;
  nShortNameLen : integer;
begin
  SetLength(sShortName, MAX_PATH);
  nShortNameLen := GetShortPathName(
    PChar(sLongName), PChar(sShortName), MAX_PATH - 1
  );
  if (0 = nShortNameLen) then
  begin
    // handle errors...
  end;
  SetLength(sShortName, nShortNameLen);
  Result := sShortName;
end;


procedure ShellExecuteEasy(const FileName : string);
begin
  ShellExecute(
    0,
    PChar('open'),
    PChar(FileName),
    nil,
    nil,
    SW_SHOW
    );
end;

function AreDblEqual(d1, d2 : double):boolean;
const
  AcceptableDelta = 0.00001;
begin
  Result := (Abs(d1 - d2)) < AcceptableDelta;
end;

function DoubleIsZero(d1 : double):boolean;
begin
  Result := AreDblEqual(d1, 0);
end;

procedure DrawGradient(StartColor, EndColor : TColor; TargetCanvas : TCanvas; TargetRect : TRect);
var
  i: Integer;
  J: Real;
  Deltas: array [0..2] of Real; // R,G,B
  R: TRect;
  LStartRGB, LEndRGB: TColor;
  LSteps: Word;
begin
  LSteps := 100;

  LStartRGB := StartColor;
  LEndRGB := EndColor;

  Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
  Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
  Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;

  TargetCanvas.Brush.Style := bsSolid;

  J := (TargetRect.Right - TargetRect.Left) / LSteps;
  for i := 0 to LSteps do begin
    R.Top := 0;
    R.Bottom := TargetRect.Bottom - TargetRect.Top;
    R.Left := Round(I * J);
    R.Right := Round((I + 1) * J);
    TargetCanvas.Brush.Color := RGB(
      Round(GetRValue(LStartRGB) + I * Deltas[0]),
      Round(GetGValue(LStartRGB) + I * Deltas[1]),
      Round(GetBValue(LStartRGB) + I * Deltas[2]));
    TargetCanvas.FillRect(R);
  end;
end;



procedure WCCLogMessage(Message: String; EventType: DWord;
  Category: Word; ID: DWord);
begin
  if FEventLogger = nil then
    FEventLogger := TEventLogger.Create(ExtractFileName(ParamStr(0)));
  FEventLogger.LogMessage(Message, EventType, Category, ID);
end;

procedure ScaleBitmap(ABitmap : TBitmap; Scale : Double; Destination : string);
var
  Temp : TBitmap;
begin
  Temp := TBitmap.Create;
  try
    Temp.Width := Trunc(ABitmap.Width * Scale);
    Temp.Height := Trunc(ABitmap.Height * Scale);
    Temp.Canvas.StretchDraw(Temp.Canvas.ClipRect, ABitmap);
    Temp.SaveToFile(Destination);
  finally
    Temp.Free;
  end;
end;

procedure ScaleBitmap(ABitmap : TBitmap; const AWidth, AHeight : integer);
var
  Temp : TBitmap;
begin
  Temp := TBitmap.Create;
  try
    Temp.Width := AHeight;
    Temp.Height := AWidth;
    Temp.Canvas.StretchDraw(Temp.Canvas.ClipRect, ABitmap);
    ABitmap.Assign(Temp);
  finally
    Temp.Free;
  end;
end;

procedure ScaleJPEG(AJPEG : TJPEGImage; const AHeight, AWidth : integer; Destination : string);
//var
//  J1, J2 : TJPEGImage;
//  B : TBitmap;
begin
//  J1 := TJPEGImage.Create;
//  J2:= TJPEGImage.Create;
//  B : TBitmap;
//  try
//    B.Assign(AJPEG);
//    B.Width := AWidth;
//    B.Height := AHeight;
//
//    B.Canvas.StretchDraw(Temp.Canvas.ClipRect, AJPEG);
//    Temp.SaveToFile(Destination);
//  finally
//    J1.Free;
//    J2.Free;
//    B.Free;
//  end;
end;

function NormalizePath(const Path : string): string;
begin
  Result := Path;
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\';
end;

function UpperFirst(const s : string): string;
//var
//  i : integer;
begin
  Result := s;
//  for i := Low(s) to High(s) do
//    if s[i] = '_' then
//      Result := Result + ' '
//    else
//      Result := Result + s[i];
end;

function UnderscoreToSpace(const s : string): string;
var
  i : integer;
begin
  for i := 1 to Length(s) do
    if s[i] = '_' then
      Result := Result + ' '
    else
      Result := Result + s[i];
end;

function IncDbl(var Value, Amount : Double):Double;
begin
  Result := Value + Amount;
end;

function CreateAccessDatabase(FileName: string): string;
var
  cat: OLEVariant;
begin
  Result := '';
  try
    cat := CreateOleObject('ADOX.Catalog');
    cat.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + FileName + ';');
    cat := NULL;
  except
    on e: Exception do Result := e.message;
  end;
end;

function GetMyDocsDir:string;
var
  Buffer : array[0..MAX_PATH] of char;
begin
  SHGetSpecialFolderPath(0, Buffer, CSIDL_PERSONAL, False);
  Result := string(Buffer);
end;

function WinGetTempPath:string;
var
  Buffer : array[0..MAX_PATH] of char;
begin
  GetTempPath(SizeOf(Buffer), Buffer);
  Result := Buffer;
end;

function WinGetTempFile:string;
begin
  Result := WinGetTempFile('');
end;

function WinGetTempFile(Prefix : string):string;
var
  Buffer : array[0..MAX_PATH] of char;
  Path : string;
begin
  Path := WinGetTempPath;
  if GetTempFileName(PChar(Path), nil, 0, Buffer) > 0 then
    Result := Buffer;
end;

function WinGetSystemDirectory:string;
var
  Buffer : array[0..MAX_PATH-1] of char;
begin
  GetSystemDirectory(Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

function WinGetComputerName:string;
var
  Buffer : array[0..MAX_PATH-1] of char;
  BufSize : Cardinal;
begin
  BufSize := SizeOf(Buffer);
  GetComputerName(Buffer, BufSize);
  Result := Buffer;
end;

function WinGetCommonAppDataDir:string;
var
  Buffer : array[0..MAX_PATH-1] of char;
begin
  SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, Buffer);
  Result := Buffer;
end;

function WinGetAppDataDir:string;
var
  Buffer : array[0..MAX_PATH-1] of char;
begin
  SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Buffer);
  Result := Buffer;
end;

procedure GetDisplayMetrics(var CX, CY : integer);
begin
  CX := GetSystemMetrics(SM_CXFULLSCREEN);
  CY := GetSystemMetrics(SM_CYFULLSCREEN);
end;

function NullStr(const s : string):string;
begin
  Result := s;
  if Length(s) = 0 then
    Result := #0;
end;

function NVL(value : Variant):boolean;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
end;

//procedure AddStringViaStream(sList : TStrings; s : string);
//var
//  SS : TStringStream;
//  TempList : TStringList;
//begin
//  if sList = nil then Exit;
//  if Length(Trim(s)) = 0 then Exit;
//  SS := TStringStream.Create(s);
//  try
//    TempList := TStringList.Create;
//    try
//      TempList.LoadFromStream(SS);
//      sList.AddStrings(TempList)
//    finally
//      TempList.Free;
//    end;
//  finally
//    SS.Free;
//  end;
//end;

//procedure AddStringViaStream(sList : TWideStrings; s : string);
//var
//  SS : TStringStream;
//  TempList : TStringList;
//begin
//  if sList = nil then Exit;
//  if Length(Trim(s)) = 0 then Exit;
//  SS := TStringStream.Create(s);
//  try
//    TempList := TStringList.Create;
//    try
//      TempList.LoadFromStream(SS);
//      sList.AddStrings(TempList)
//    finally
//      TempList.Free;
//    end;
//  finally
//    SS.Free;
//  end;
//end;

procedure WCCTrace(const Msg : string);
begin
  {$IFOPT D+}
  if IsDebugged then
    TraceMsg(Format('%s - %s',[FormatDateTime('hh:nn:ss.zzz',Now),Msg]))
  else
    WCCLogMessage(
      Format('%s - %s',[FormatDateTime('hh:nn:ss.zzz',Now),Msg]),
      EVENTLOG_INFORMATION_TYPE);
  {$ENDIF}
end;

procedure WCCTraceStrings(sList : TStrings);
{$IFOPT D+}
var
  i : integer;
{$ENDIF}
begin
  {$IFOPT D+}
  if sList = nil then Exit;
  for i := 0 to sList.Count - 1 do
    if IsDebugged then TraceMsg(sList[i])
    else WCCLogMessage(sList[i], EVENTLOG_INFORMATION_TYPE);
  {$ENDIF}
end;

procedure CheckVarInit(ObjClass : TObject);
begin
  if ObjClass = nil then
    raise Exception.CreateFmt('Object of type %s not initialized',[ObjClass.ClassName]);
end;

procedure CheckDBID(const ID : integer);
begin
  if ID <= 0 then
    raise Exception.CreateFmt('%d does not appear to be a valid database identifier',[ID]);
end;

procedure CheckStrID(const strID : string);
begin
  if Length(strID) = 0 then
    raise Exception.CreateFmt('%s does not appear to be a valid database identifier',[strID]);
end;

function ConfirmYN(const Msg : string):boolean;
begin
  MessageBeep(MB_ICONQUESTION);
  Result := MessageDlg(Msg,mtConfirmation,[mbYes,mbNo],0) = mrYes;
end;

function ConfirmTF(const Msg : string):boolean;
begin
  MessageBeep(MB_ICONQUESTION);
  Result := MessageDlg(Msg,mtConfirmation,[mbOK,mbCancel],0) = mrOK;
end;

function WarnYN(const Msg : string):boolean;
begin
  MessageBeep(MB_ICONASTERISK);
  Result := MessageDlg(Msg,mtWarning,[mbYes,mbNo],0) = mrYes;
end;

function WarnYNFmt(const Msg : string; Args : array of const):boolean;
begin
  Result := WarnYN(Format(Msg, Args));
end;

procedure WarnMsg(const Msg : string);
begin
  MessageBeep(MB_ICONASTERISK);
  MessageDlg(Msg,mtWarning,[mbOK],0);
end;

procedure WarnMsgFmt(const Msg : string; Args : array of const);
begin
  WarnMsg(Format(Msg, Args));
end;

procedure ErrorMsg(const Msg : string);
begin
  WCCLogMessage(Msg, EVENTLOG_ERROR_TYPE);
  MessageBeep(MB_ICONEXCLAMATION);
  MessageDlg(Msg,mtError,[mbOK],0);
end;

procedure ErrorMsgFmt(const Msg : string; Args : array of const);
begin
  ErrorMsg(Format(Msg, Args));
end;

procedure InfoMsg(const Msg : string);
begin
  MessageBeep(MB_OK);
  MessageDlg(Msg, mtInformation,[mbOK],0);
end;

procedure InfoMsgFmt(const Msg : string; Args : array of const);
begin
  InfoMsg(Format(Msg,Args));
end;

function iif_str(const eval : boolean; const s1, s2 : string):string;
begin
  Result := s1;
  if not eval then
    Result := s2;
end;

function iif_chr(const eval : boolean; const s1, s2 : Char):Char;
begin
  Result := s1;
  if not eval then
    Result := s2;
end;

function iif_int(const eval : boolean; const i1, i2 : integer):integer;
begin
  Result := i1;
  if not eval then
    Result := i2;
end;

function iif_dbl(const eval : boolean; const d1, d2 : double):double;
begin
  Result := d1;
  if not eval then
    Result := d2;
end;

function iif_sng(const eval : boolean; const d1, d2 : single):single;
begin
  Result := d1;
  if not eval then
    Result := d2;
end;

function iif_var(const eval : boolean; const v1, v2 : variant):variant;
begin
  Result := v1;
  if not eval then
    Result := v2;
end;

function RegReadString(const Key, Value : string):string;
begin
  Result := '';
  with TRegistry.Create do try
    if OpenKey(Key,False) then
      if ValueExists(Value) then
        Result := ReadString(Value);
  finally
    Free;
  end;
end;

function RegReadInteger(const Key, Value : string):integer;
begin
  Result := -1;
  with TRegistry.Create do try
    if OpenKey(Key,False) then
      if ValueExists(Value) then
        Result := ReadInteger(Value);
  finally
    Free;
  end;
end;

procedure RegWriteString(const Key, Value, Data : string;
  Force : boolean = False);
begin
  with TRegistry.Create do try
    if OpenKey(Key,Force) then
      if Force then
        WriteString(Value,Data)
      else if ValueExists(Value) then
        WriteString(Value,Data);
  finally
    Free;
  end;
end;

procedure RegWriteInteger(const Key, Value : string; const Data : integer;
  Force : boolean = False);
begin
  with TRegistry.Create do try
    if OpenKey(Key,Force) then
      if Force then
        WriteInteger(Value,Data)
      else if ValueExists(Value) then
        WriteInteger(Value,Data);
  finally
    Free;
  end;
end;

function RegReadBoolean(const Key, Value : string):Boolean;
begin
  Result := False;
  with TRegistry.Create do try
    if OpenKey(Key,False) then
      if ValueExists(Value) then
        Result := ReadBool(Value);
  finally
    Free;
  end;
end;

procedure RegWriteBoolean(const Key, Value : string; const Data : Boolean; Force : boolean = False);
begin
  with TRegistry.Create do try
    if OpenKey(Key,Force) then
      if Force then
        WriteBool(Value,Data)
      else if ValueExists(Value) then
        WriteBool(Value,Data);
  finally
    Free;
  end;
end;

{$IFDEF DIRECTX}
procedure TranslateD3DColor(const Color : TD3DColor; var R, G, B : integer);
begin
  R := (Color shr 16) and $ff;
  G := (Color shr 8) and $ff;
  B := Color and $ff;
end;

function ColorToD3DColor(ATColor : TColor):TD3DColor;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(ATColor);
  Result := D3DCOLOR_XRGB(Temp.Red,Temp.Green,Temp.Blue);
end;

function D3DColorToColor(AD3DColor : TD3DColor):TColor;
var
  R, G, B : integer;
  Temp: TColorRec;
begin
  TranslateD3DColor(AD3DColor, R, G, B);
  Temp.Red := R;
  Temp.Green := G;
  Temp.Blue := B;
  Temp.Flag := 0;
  Result := Temp.Value
end;
{$ENDIF}

function ExtractFileNameURL(URL : string):string;
var
  s : string;
begin
  { strip from the left until the last / is removed the return the rest }
  { This probably won't conform to any standard }
  Result := '';
  s := URL;
  while Pos('/', s) > 0 do
    s := Copy(s, 2, Length(s) - 1);
  Result := s;
end;

function RefInterface(const Intf : IUnknown): Pointer;
begin
  Intf._AddRef;
  Result := Pointer(Intf);
end;

procedure ReleaseInterface(P : Pointer);
var
  Intf : IUnknown;
begin
  Pointer(Intf) := P;
end;

function RefString(const s : string):Pointer;
var
  local : string;
begin
  local := s;
  Result := Pointer(local);
  Pointer(local) := nil;
end;

procedure ReleaseString(P : Pointer);
var
  local : string;
begin
  Pointer(local) := P;
end;

function UpperChar(const C: Char): Char;
begin
  Result := C;
  if (Result >= 'a') and (Result <= 'z') then
    Dec(Result, 32);
end;

function SilentExecSQL(ADOQuery : TADOQuery):integer;
begin
  Result := 0;
  try
    Result := ADOQuery.ExecSQL;
  except
  end
end;

procedure SetADODateTimeParameter(P : TParameter; DT : TDateTime);
var
  PO : _Parameter;
begin
  PO := P.ParameterObject;
  PO.Type_ := adVarChar;
  PO.Size := 26;
  PO.Attributes := 0;
  P.DataType := ftString;
  P.Size := 26;
  P.Value := FormatDateTime('yyyy-mm-dd-hh.nn.ss.zzz000',DT);
end;

function StrToBool(const s : string):boolean;
begin
  Result := Trim(s) = 'True';
end;

function BoolToStr(const b : boolean):string;
begin
  Result := iif_str(b,'True','False');
end;

procedure WCCShowWindow(AForm : TForm);
var
  Wnd, TopWnd : HWND;
begin
  Wnd := AForm.Handle;
  SetForegroundWindow(Wnd);
  TopWnd := GetLastActivePopup(Wnd);
  if (TopWnd <> 0) and (TopWnd <> Wnd) and
      IsWindowVisible(TopWnd) and IsWindowEnabled(TopWnd) then
    BringWindowToTop(TopWnd)
  else
    BringWindowToTop(Wnd);
end;

{ TDelimParser }

constructor TDelimParser.Create(const s: string; const delim: char);
begin
  FKeys := TStringList.Create;
  FValues := TStringList.Create;
  FDelim := delim;
  FSource := s;
  Decompose;
end;

procedure TDelimParser.Decompose;
var
  i : integer;
  sTemp : TStringList;
  s1, s2 : string;
begin
  sTemp := TStringList.Create;
  try
    StrToStrings(FSource,FDelim,sTemp);
    for i := 0 to sTemp.Count -1 do begin
      s1 := sTemp[i];
      s2 := StrToken(s1,'=');
      FKeys.Add(s2);
      FValues.Add(s1);
    end;
  finally
    sTemp.Free;
  end;
end;

destructor TDelimParser.Destroy;
begin
  FKeys.Free;
  FValues.Free;
  inherited;
end;

function TDelimParser.GetElements(index: string): string;
begin
  Result := '';
  if FKeys.IndexOf(index) > -1 then
    Result := FValues[FKeys.IndexOf(index)];
end;

function IsDebugged: Boolean;
var
  IsDebuggerPresent: function: Boolean; stdcall;
  KernelHandle: THandle;
  P: Pointer;
begin
  KernelHandle := GetModuleHandle(kernel32);
  @IsDebuggerPresent := GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if Assigned(IsDebuggerPresent) then // Win98+/NT4+ only
    Result := IsDebuggerPresent
  else
  begin // Win9x uses thunk pointer outside the module when under a debugger
    P := GetProcAddress(KernelHandle, 'GetProcAddress');
    Result := (DWORD(P) < KernelHandle);
  end;
end;


initialization

finalization
  if FEventLogger <> nil then
    FreeAndNil(FEventLogger);


end.
