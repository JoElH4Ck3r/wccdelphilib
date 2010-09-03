unit DBLogin;

interface

{$I WCC.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, Registry, ADODB, WestCoastCode;

type
  EDBLoginDlgError = class(Exception);

  TLoginFailureEvent = procedure(Sender : TObject;const ErrorMessage : string) of object;

  TDBLoginDlg = class;

  TfLogin = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    eUsername: TEdit;
    ePassword: TEdit;
    bOK: TButton;
    bCancel: TButton;
    Label3: TLabel;
    comboDSN: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDBLoginDlgOwner : TDBLoginDlg;
    function GetDatabase: string;
    function GetPassword: string;
    function GetUsername: string;
    procedure LoadLast;
  public
    constructor Create(AOwner : TComponent); override;
    procedure SaveLast;
    property UserName : string read GetUsername;
    property Password : string read GetPassword;
    property Database : string read GetDatabase;
  end;

  TCommandLineSwitches = class(TPersistent)
  private
    FUsername: string;
    FPassword: string;
    FDatabase: string;
  public
    constructor Create;
  published
    property Username : string read FUsername write FUsername;
    property Password : string read FPassword write FPassword;
    property Database : string read FDatabase write FDatabase;
  end;

  TDBLoginDlg = class(TComponent)
  private
    FUsername: string;
    FPassword: string;
    FDatabase: string;
    FMaxFailures: integer;
    FAllowCommandLine : boolean;
    FOnLoginSuccess: TNotifyEvent;
    FOnLoginFailure: TLoginFailureEvent;
    FCommandLineSwitches : TCommandLineSwitches;
    FADOConnection: TADOConnection;
    FRegSubKey: string;
    FRegAppRoot: string;
    FSchema: string;
    FAutoSetSchema: boolean;
    FOrigAfterConnectEvent : TNotifyEvent;
    F : TfLogin;
    procedure SetOnLoginFailure(const Value: TLoginFailureEvent);
    procedure SetOnLoginSuccess(const Value: TNotifyEvent);
    function GetCommandLineSwitches: TCommandLineSwitches;
    procedure SetCommandLineSwitches(const Value: TCommandLineSwitches);
    function ParamLogin(var U, P, D: string): boolean;
    function Login(F: TfLogin;var Attempts : integer;  CheckParams : boolean = True): boolean;
    function Connect(const U, P, D: string): boolean;
    function GetRegKey: string;
    procedure DoAfterConnect(Sender: TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean;
    property Username : string read FUsername;
    property Password : string read FPassword;
    property Database : string read FDatabase;
    property RegKey : string read GetRegKey;
    function TestConnection(const U, P, D: string):boolean;
  published
    { Properties }
    property RegAppRoot : string read FRegAppRoot write FRegAppRoot;
    property RegSubKey : string read FRegSubKey write FRegSubKey;
    property MaxFailures : integer read FMaxFailures write FMaxFailures;
    property AllowCommandLine : boolean read FAllowCommandLine write FAllowCommandLine;
    property CommandLineSwitches : TCommandLineSwitches read GetCommandLineSwitches write SetCommandLineSwitches;
    property ADOConnection : TADOConnection read FADOConnection write FADOConnection;
    property Schema : string read FSchema write FSchema;
    property AutoSetSchema : boolean read FAutoSetSchema write FAutoSetSchema;
    { Events }
    property OnLoginSuccess : TNotifyEvent read FOnLoginSuccess write SetOnLoginSuccess;
    property OnLoginFailure : TLoginFailureEvent read FOnLoginFailure write SetOnLoginFailure;
  end;

var
  fLogin: TfLogin;

implementation

uses ODBC,
//     {$IFDEF DELPHI9}
//     jcl_rip
//     {$ELSE}
     JclStrings,
//     {$ENDIF} ,
     WCCUtil;

const
  ConStr = 'Provider=MSDASQL.1;Password=%s;Persist Security Info=True;User ID=%s;Data Source=%s';
  RegLastUser = 'LastUser';
  RegLastDB = 'LastDB';

  DefaultMaxAttempts = 5;
  DefaultRegKey = 'Connection';

{$R *.dfm}

{ TfLogin }

function TfLogin.GetDatabase: string;
begin
  Result := comboDSN.Items[comboDSN.itemIndex];
end;

function TfLogin.GetPassword: string;
begin
  Result := ePassword.Text;
end;

function TfLogin.GetUsername: string;
begin
  Result := eUserName.Text;
end;

procedure TfLogin.LoadLast;
var
  s : string;
begin
  with TRegistry.Create do try
    if OpenKey(FDBLoginDlgOwner.RegKey, True) then begin
      eUserName.Text := ReadString(RegLastUser);
      s := ReadString(RegLastDB);
    end;
  finally
    Free;
  end;
  GetDSNList(comboDSN.Items, [C_DB2_7_2_DriverName, C_INTERBASE_56_DriverName]);
  comboDSN.ItemIndex := comboDSN.Items.IndexOf(s);
end;

procedure TfLogin.SaveLast;
begin
  with TRegistry.Create do try
    if OpenKey(FDBLoginDlgOwner.RegKey, True) then begin
      WriteString(RegLastUser, eUserName.Text);
      WriteString(RegLastDB, comboDSN.Items[comboDSN.itemIndex]);
    end;
  finally
    Free;
  end;
end;

procedure TfLogin.FormCreate(Sender: TObject);
begin
  LoadLast;
end;

procedure TfLogin.FormShow(Sender: TObject);
begin
  if Length(eUserName.Text) > 0 then
    ePassword.SetFocus();
end;

constructor TfLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (AOwner is TDBLoginDlg) then
    raise EDBLoginDlgError.Create('Invalid instantiation of TfLogin');
  FDBLoginDlgOwner := AOwner as TDBLoginDlg;
end;

{ TDBLoginDlg }

constructor TDBLoginDlg.Create(AOwner: TComponent);
begin
  inherited;
  FAllowCommandLine := True;
  FSchema := 'LYNX';
  FAutoSetSchema := True;
  FRegSubKey := DefaultRegKey;
  FRegAppRoot := WWC_RootRegKey;
  FMaxFailures := DefaultMaxAttempts;
  FUsername := '';
  FPassword := '';
  FDatabase := '';
  FCommandLineSwitches := TCommandLineSwitches.Create;
end;

destructor TDBLoginDlg.Destroy;
begin
  FCommandLineSwitches.Free;
  inherited;
end;

function TDBLoginDlg.ParamLogin(var U, P, D : string):boolean;
var
  i : integer;
  Flag, Value : string;
begin
  U := '';
  P := '';
  D := '';
//  Result := False;
  { Need all three to proceed }
  for i := 1 to ParamCount do begin
    Value := ParamStr(i);
    Flag := StrToken(Value,':');
    if CompareText(Flag,FCommandLineSwitches.Database) = 0 then D := Value
    else if CompareText(Flag,FCommandLineSwitches.Username) = 0 then U := Value
    else if CompareText(Flag,FCommandLineSwitches.Password) = 0 then P := Value;
  end;
  Result := ((Length(D)>0) and (Length(U)>0) and (Length(P)>0));
end;

function TDBLoginDlg.Execute: boolean;
var
//  F : TfLogin;
  Attempts : integer;
begin
  Attempts := -1; // this gets inc-ed to 0 on the first call
//  Result := False;
  F := TfLogin.Create(Self);
  try
    Result := Login(F,Attempts,FAllowCommandLine);
    if Result then F.SaveLast;
  finally
    F.Free;
  end;
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

function TDBLoginDlg.Login(F : TfLogin;var Attempts : integer; CheckParams : boolean = True):boolean;
var
  U, P, D : string;
begin
  Result := False;
  Inc(Attempts);
  try
    if Attempts >= FMaxFailures then
      raise EDBLoginDlgError.CreateFmt('Error, maximum login attempts %d',[FMaxFailures]);
    { Check for command line params and auto login if necassary }
    try
      if CheckParams then begin
        Dec(Attempts); // Param login should count as a attempt
        if ParamLogin(U, P, D) then begin
          if Connect(U, P, D) then
            Result := True
          else
            Result := Login(F,Attempts,False);
        end
        else begin
//
          Result := Login(F,Attempts,False);
        end;
      end
      else
      begin
        F.ePassword.Text := '';
        if F.ShowModal = mrOK then begin
          U := F.UserName;
          P := F.Password;
          D := F.Database;
          if Connect(U, P, D) then
            Result := True
          else begin
            Result := Login(F,Attempts,False);
          end;
        end
        else
          raise EDBLoginDlgError.Create('Login Terminated');
      end;
    finally
      if Result then
        if Assigned(FOnLoginSuccess) then
            FOnLoginSuccess(Self);
    end;
  except
    on E : Exception do begin
      if E is EDBLoginDlgError then
      begin
        Result := False;
        if Assigned(FOnLoginFailure) then
          FOnLoginFailure(Self,E.Message);
      end
      else
        raise;
    end;
  end;
end;

function TDBLoginDlg.TestConnection(const U, P, D: string): boolean;
begin
  Result := False;
  if FADOConnection <> nil then begin
    Result := Connect(U,P,D);
    if FADOConnection.Connected then
      FADOConnection.Connected := False;
  end;
end;

function TDBLoginDlg.Connect(const U, P, D: string): boolean;
resourcestring
  DSNError = '[Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified';
  UserPassError = '[IBM][CLI Driver] SQL1403N  The username and/or password supplied is incorrect.  SQLSTATE=08004';
begin
  if FADOConnection = nil then
    raise EDBLoginDlgError.Create('Error, ADO database object not specified');
  with FADOConnection do begin
    if Connected then
      Close;
    ConnectionString := Format(ConStr,[P,U,D]);
    try
      { Divert AfterConnectEvent }
      if Assigned(AfterConnect) then
        FOrigAfterConnectEvent := AfterConnect;
      AfterConnect := DoAfterConnect;
      { Try to open connection }
      Open;
      { Reset AfterConnectEvent }
      if Assigned(FOrigAfterConnectEvent) then
        AfterConnect := FOrigAfterConnectEvent;
      {}
      FUserName := Uppercase(U);
      FPassword := P;
      FDatabase := D;
    except
      on E : Exception do begin
        if CompareText(E.Message,DSNError) = 0 then
          ErrorMsg('Invalid Database Name')
        else if CompareText(E.Message,UserPassError) = 0 then
          ErrorMsg('Invalid Username or Password')
        else
          ErrorMsg(E.Message);
        //WCCShowWindow(F);
      end;
    end;
    Result := Connected;
  end;
end;

procedure TDBLoginDlg.DoAfterConnect(Sender : TObject);
begin
  if AutoSetSchema then
    with TADOQuery.Create(nil) do try
      Connection := FADOConnection;
      SQL.Add(Format('SET SCHEMA %s',[Schema]));
      ExecSQL;
      SQL.Clear;
      SQL.Add(Format('SET PATH SYSFUN,%s',[Schema]));
      ExecSQL;
    finally
      Free;
    end;
  if Assigned(FOrigAfterConnectEvent) then
    FOrigAfterConnectEvent(Sender);
end;

function TDBLoginDlg.GetCommandLineSwitches: TCommandLineSwitches;
begin
  Result := FCommandLineSwitches;
end;

procedure TDBLoginDlg.SetCommandLineSwitches(const Value: TCommandLineSwitches);
begin
  FCommandLineSwitches.Assign(Value);
end;

procedure TDBLoginDlg.SetOnLoginFailure(const Value: TLoginFailureEvent);
begin
  FOnLoginFailure := Value;
end;

procedure TDBLoginDlg.SetOnLoginSuccess(const Value: TNotifyEvent);
begin
  FOnLoginSuccess := Value;
end;

function TDBLoginDlg.GetRegKey: string;
begin
  Result := FRegAppRoot + FRegSubKey;
end;

{ TCommandLineSwitches }

constructor TCommandLineSwitches.Create;
begin
  inherited Create;
  FUsername := '-u';
  FPassword := '-p';
  FDatabase := '-d';
end;

end.
