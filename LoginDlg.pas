unit LoginDlg;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, LoginForm, CommandLineSwitches, ADODB,
  WestCoastCode, DBConnection, CustomWCCConnection;

type
  TWCCLoginDlg = class;

//  TSecondaryConnection = class(TCollectionItem)
//  private
//    FEnabled: boolean;
//    FDB: TWCCConnection;
//    procedure SetDB(const Value: TWCCConnection);
//    procedure SetEnabled(const Value: boolean);
//  protected
//    function GetDisplayName: string; override;
//  public
//    constructor Create(Collection: TCollection); override;
//    procedure Assign(Source: TPersistent); override;
//  published
//    property DB : TWCCConnection read FDB write SetDB;
//    property Enabled : boolean read FEnabled write SetEnabled;
//  end;

//  TSecondaryConnections = class(TCollection)
//  private
//    FWCCLoginDlg : TWCCLoginDlg;
//    function GetItem(Index: Integer): TSecondaryConnection;
//    procedure SetItem(Index: Integer; const Value: TSecondaryConnection);
//  protected
//    function GetOwner: TPersistent; override;
//    procedure Update(Item: TCollectionItem); override;
//  public
//    constructor Create(LoginDlg: TWCCLoginDlg);
//    function Add: TSecondaryConnection;
//    function AddItem(Item: TSecondaryConnection; Index: Integer): TSecondaryConnection;
//    function Insert(Index: Integer): TSecondaryConnection;
//    property Items[Index: Integer]: TSecondaryConnection read GetItem write SetItem; default;
//  end;

  TWCCLoginDlg = class(TComponent)
  private
    FUsername: string;
    FPassword: string;
    FDatabase: string;
    FMaxFailures: integer;
    FAllowCommandLine : boolean;
    FOnLoginSuccess: TNotifyEvent;
    FOnLoginFailure: TLoginFailureEvent;
    FCommandLineSwitches : TCommandLineSwitches;
    FADOConnection: TWCCConnection;
    FRegSubKey: string;
    FRegAppRoot: string;
    FSchema: string;
    FAutoSetSchema: boolean;
    FOrigAfterConnectEvent : TNotifyEvent;
    FDatabaseServers : TDatabaseServers;
    FDSNList : TStringList;
    FAllowOSAuthentication: boolean;

    FTempUser : string;
    FPath: string; // store the user temporarily for the DB AfterConnect event

//    FSecondaryConnections: TSecondaryConnections;
//    F : TFDBLogin;
    procedure SetOnLoginFailure(const Value: TLoginFailureEvent);
    procedure SetOnLoginSuccess(const Value: TNotifyEvent);
    function GetCommandLineSwitches: TCommandLineSwitches;
    procedure SetCommandLineSwitches(const Value: TCommandLineSwitches);
    function ParamLogin(var U, P, D, S, F : string): boolean;
    function Login(F: TFDBLogin;var Attempts : integer;  CheckParams : boolean = True): boolean; overload;
    function Login(F: TFDBLogin; CheckParams : boolean = True): boolean; overload;
    function Connect(const U, P, D, S, F: string): boolean;
    function GetRegKey: string;
    procedure DoAfterConnect(Sender: TObject);
    procedure SaveSuccessfulLogin(const ADB, AUsername, ASchema, APath : string);
    procedure SetLastLogin(ALogin : TFDBLogin);
    //function PerformConnect(ALogin: TFDBLogin; const U, P, D, S: string): boolean;
    procedure RefreshDSNList;
    procedure SetAllowOSAuthentication(const Value: boolean);
//    procedure SetSecondaryConnections(const Value: TSecondaryConnections);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean; overload;
    function Execute(ADB : TWCCConnection) : boolean; overload;
    property Username : string read FUsername;
    property Password : string read FPassword;
    property Database : string read FDatabase;
    property RegKey : string read GetRegKey;
    function TestConnection(const U, P, D, S, F: string):boolean;
    procedure ShowConnectionProperties;
  published
    { Properties }
    property RegAppRoot : string read FRegAppRoot write FRegAppRoot;
    property RegSubKey : string read FRegSubKey write FRegSubKey;
    property MaxFailures : integer read FMaxFailures write FMaxFailures;
    property AllowCommandLine : boolean read FAllowCommandLine write FAllowCommandLine;
    property AllowOSAuthentication : boolean read FAllowOSAuthentication write SetAllowOSAuthentication;
    property CommandLineSwitches : TCommandLineSwitches read GetCommandLineSwitches write SetCommandLineSwitches;
    property ADOConnection : TWCCConnection read FADOConnection write FADOConnection;
  //  property SecondaryConnections : TSecondaryConnections read FSecondaryConnections write SetSecondaryConnections;
    {}
    property Schema : string read FSchema write FSchema;
    property Path : string read FPath write FPath;
    property AutoSetSchema : boolean read FAutoSetSchema write FAutoSetSchema;
    {}
    property DatabaseServers : TDatabaseServers read FDatabaseServers write FDatabaseServers;
    { Events }
    property OnLoginSuccess : TNotifyEvent read FOnLoginSuccess write SetOnLoginSuccess;
    property OnLoginFailure : TLoginFailureEvent read FOnLoginFailure write SetOnLoginFailure;
  end;


implementation

uses JclStrings, WCCUtil, LoginConst, Registry, ODBC, DBConnectionProperties;

{ TDBLoginDlg }

constructor TWCCLoginDlg.Create(AOwner: TComponent);
begin
  inherited;
//  FSecondaryConnections := TSecondaryConnections.Create(Self);
  FAllowCommandLine := True;
  FAllowOSAuthentication := False;
  FSchema := 'LYNX';
  FPath := 'LYNX';
  FAutoSetSchema := True;
  FRegSubKey := DefaultRegKey;
  FRegAppRoot := WWC_RootRegKey;
  FMaxFailures := DefaultMaxAttempts;
  FUsername := '';
  FPassword := '';
  FDatabase := '';
  FDSNList := TStringList.Create;
  FCommandLineSwitches := TCommandLineSwitches.Create;
  FDatabaseServers := [dsDB2, dsMSSQL];
end;

destructor TWCCLoginDlg.Destroy;
begin
  FCommandLineSwitches.Free;
  FDSNList.Free;
//  FSecondaryConnections.Free;
  inherited;
end;

procedure TWCCLoginDlg.RefreshDSNList;
var
  a : array of string;
begin
  FDSNList.Clear;

  if dsDB2 in FDatabaseServers then begin
    SetLength(a, Length(a)+1);
    a[Length(a)-1] := C_DB2_7_2_DriverName;
  end;

  if dsMSSQL in FDatabaseServers then begin
    SetLength(a, Length(a)+2);
    a[Length(a)-2] := C_SQL_Server;
    a[Length(a)-1] := C_SQL_Native_Client;
  end;

  GetDSNList(FDSNList, a);
end;

function TWCCLoginDlg.ParamLogin(var U, P, D, S, F : string):boolean;
var
  i : integer;
  Flag, Value : string;
begin
  U := '';
  P := '';
  D := '';
  S := '';
  F := '';
//  Result := False;
  { Need all three to proceed }
  for i := 1 to ParamCount do begin
    Value := ParamStr(i);
    Flag := StrToken(Value,':');
    if CompareText(Flag,FCommandLineSwitches.Database) = 0 then D := Value
    else if CompareText(Flag,FCommandLineSwitches.Username) = 0 then U := Value
    else if CompareText(Flag,FCommandLineSwitches.Password) = 0 then P := Value
    else if CompareText(Flag,FCommandLineSwitches.Schema) = 0 then S := Value
    else if CompareText(Flag,FCommandLineSwitches.Path) = 0 then F := Value;
  end;
  Result := ((Length(D)>0) and (Length(U)>0) and (Length(P)>0));
end;

function TWCCLoginDlg.Execute: boolean;
var
  FLogin : TFDBLogin;
  Attempts : integer;
begin
  Result := False;
  Attempts := -1; // this gets inc-ed to 0 on the first call
  RefreshDSNList;
  FLogin := TFDBLogin.Create(Self, FDSNList, Connect);
  try
    SetLastLogin(FLogin);
    Result := Login(FLogin, Attempts, FAllowCommandLine);
    if Result then
      SaveSuccessfulLogin(FLogin.Database, FLogin.Username, FLogin.Schema, FLogin.Path);
  finally
    FLogin.Free;
  end;
end;

function TWCCLoginDlg.Execute(ADB: TWCCConnection): boolean;
var
  FLogin : TFDBLogin;
begin
  { ADB is the object this whole process acts upon }
  { Instantiate the connect for. }
  Result := False;
  RefreshDSNList;
  FLogin := TFDBLogin.Create(Self, FDSNList, Connect);
  try
    FADOConnection := ADB;
    SetLastLogin(FLogin);
    Result := Login(FLogin, FAllowCommandLine);
    if Result then
      SaveSuccessfulLogin(FLogin.Database, FLogin.Username, FLogin.Schema, FLogin.Path);
  finally
    FLogin.Free;
  end;
end;

procedure TWCCLoginDlg.SetLastLogin(ALogin : TFDBLogin);
begin
  if ALogin = nil then Exit;
  with TRegistry.Create do try
    if OpenKey(RegKey, True) then begin
      ALogin.SetSaveValues(
        ReadString(RegLastDB),
        ReadString(RegLastUser),
        ReadString(RegLastSchema),
        ReadString(RegLastPath)
        );
    end;
    if FADOConnection is TWCCConnection then begin
      (FADOConnection as TWCCConnection).Username := ALogin.UserName;
      (FADOConnection as TWCCConnection).SchemaName := ALogin.Schema;
      (FADOConnection as TWCCConnection).PathName := ALogin.Path;
    end;
  finally
    Free;
  end;
end;

procedure TWCCLoginDlg.SaveSuccessfulLogin(const ADB, AUsername, ASchema, APath : string);
begin
  with TRegistry.Create do try
    if OpenKey(RegKey, True) then begin
      WriteString(RegLastUser, AUsername);
      WriteString(RegLastDB, ADB);
      WriteString(RegLastSchema, ASchema);
      WriteString(RegLastPath, APath);
    end;
  finally
    Free;
  end;
end;

function TWCCLoginDlg.Login(F: TFDBLogin; CheckParams: boolean): boolean;
var
  U, P, D, S, FP : string;
  ConnectResult : boolean;
begin
  Result := False;
  ConnectResult := False;
  try
    { Check for command line params and auto login if necassary }
    try
      if CheckParams then begin
        if ParamLogin(U, P, D, S, FP) then begin
          Result := Connect(U, P, D, S, FP);
          if not Result then
            Result := Login(F, False);
        end
        else begin
          Result := Login(F,False);
        end;
      end
      else
      begin
        Result := F.ShowModal = mrOK;
        if not Result then
          raise EDBLoginDlgError.Create('Login Terminated');
//          Result := Login(F,False)

//        if F.ShowModal = mrOK then begin
//          { Attempt Login }
//          Result := Connect(F, F.UserName, F.Password, F.Database);
//          if not Result then
//            Result := Login(F,False);
//        end
//        else
//          raise EDBLoginDlgError.Create('Login Terminated');
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

function TWCCLoginDlg.Login(F : TFDBLogin;var Attempts : integer; CheckParams : boolean = True):boolean;
var
  //U, P, D : string;
  ConnectResult : boolean;
begin
  Result := False;
  ConnectResult := False;
  Inc(Attempts);
  try
//    if Attempts >= FMaxFailures then
//      raise EDBLoginDlgError.CreateFmt('Error, maximum login attempts %d',[FMaxFailures]);
    { Check for command line params and auto login if necassary }
    try
//      if CheckParams then begin
//        Dec(Attempts); // Param login should count as a attempt
//        if ParamLogin(U, P, D) then begin
//          if PerformConnect(U, P, D) then
//            Result := True
//          else
//            Result := Login(F,Attempts,False);
//        end
//        else begin
////
//          Result := Login(F,Attempts,False);
//        end;
//      end
//      else
//      begin
//        F.ePassword.Text := '';
//        if F.ShowModal = mrOK then begin
//          U := F.UserName;
//          P := F.Password;
//          D := F.Database;
//          { Attempt Login }
//          F.BeginLogin;
//          try
//            ConnectResult := PerformConnect(U, P, D);
//          finally
//            F.EndLogin;
//          end;
//          {  }
//          if ConnectResult then
//            Result := True
//          else begin
//            Result := Login(F,Attempts,False);
//          end;
//        end
//        else
//          raise EDBLoginDlgError.Create('Login Terminated');
//      end;
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

function TWCCLoginDlg.TestConnection(const U, P, D, S, F: string): boolean;
begin
  Result := False;
  if FADOConnection <> nil then begin
    Result := Connect(U,P,D,S,F);
    if FADOConnection.Connected then
      FADOConnection.Connected := False;
  end;
end;

//function TWCCLoginDlg.PerformConnect(ALogin : TFDBLogin; const U, P, D, S: string):boolean;
//begin
//  ALogin.BeginLogin;
//  try
//    Result := Connect(U,P,D,S);
//  finally
//    ALogin.EndLogin;
//  end;
//end;

function TWCCLoginDlg.Connect(const U, P, D, S, F: string): boolean;
resourcestring
  DSNError = '[Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified';
  UserPassError = '[IBM][CLI Driver] SQL1403N  The username and/or password supplied is incorrect.  SQLSTATE=08004';
begin
  if FADOConnection = nil then
    raise EDBLoginDlgError.Create('Error, ADO database object not specified');

  { TruckMate and Schema... }
  {  }
  if FADOConnection.SetSchema or FADOConnection.SetPath or AutoSetSchema then
    if Length(S) = 0 then
      raise Exception.Create('Schema cannot be blank use the advanced options to specify a database schema.');

  FTempUser := UpperCase(U);

  with FADOConnection do begin
    if Connected then
      Close;
    ConnectionString := Format(ConStr,[P,U,D]);
//    InfoMsg(ConnectionString);
    FSchema := S;

    FADOConnection.SchemaName := S;
    FADOConnection.PathName := F;
    try
      { Divert AfterConnectEvent }
      if Assigned(AfterConnect) then
        FOrigAfterConnectEvent := AfterConnect;
      AfterConnect := DoAfterConnect;
      { Try to open connection }
      try
//        InfoMsg('TWCCLoginDlg.Connect 1');

        Open;
        //Open(U,P);

//        InfoMsg('TWCCLoginDlg.Connect 2');

        FUserName := Uppercase(U);
        FPassword := P;
        FDatabase := D;
        
      finally
        { Reset AfterConnectEvent }
        AfterConnect := nil;
        if Assigned(FOrigAfterConnectEvent) then
          AfterConnect := FOrigAfterConnectEvent
      end;
      {}
//      FUserName := Uppercase(U);
//      FPassword := P;
//      FDatabase := D;
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

procedure TWCCLoginDlg.DoAfterConnect(Sender : TObject);
begin
  // Thos will get properly after the connection is made
  // until then we do this to that and application level OnAfterConnect events
  // will have the correct user.
  FADOConnection.UserName := FTempUser;

  if AutoSetSchema then begin

    if FADOConnection.DatabaseServerType = dsDB2 then

      { If the database has not already set the schema }
      if ((not FADOConnection.SetSchema) and (Length(Schema) > 0)) then
        with TADOQuery.Create(nil) do try
          Connection := FADOConnection;
          SQL.Add(Format('SET SCHEMA %s',[Schema]));
          ExecSQL;
        finally
          Free;
        end;

      if ((not FADOConnection.SetPath) and (Length(Schema) > 0)) then
        with TADOQuery.Create(nil) do try
          Connection := FADOConnection;
          SQL.Add(Format('SET PATH SYSFUN,%s',[Schema]));
          ExecSQL;
        finally
          Free;
        end;

  end;

  if Assigned(FOrigAfterConnectEvent) then
    FOrigAfterConnectEvent(Sender);
end;

function TWCCLoginDlg.GetCommandLineSwitches: TCommandLineSwitches;
begin
  Result := FCommandLineSwitches;
end;

procedure TWCCLoginDlg.SetCommandLineSwitches(const Value: TCommandLineSwitches);
begin
  FCommandLineSwitches.Assign(Value);
end;

procedure TWCCLoginDlg.SetOnLoginFailure(const Value: TLoginFailureEvent);
begin
  FOnLoginFailure := Value;
end;

procedure TWCCLoginDlg.SetOnLoginSuccess(const Value: TNotifyEvent);
begin
  FOnLoginSuccess := Value;
end;

function TWCCLoginDlg.GetRegKey: string;
begin
  Result := FRegAppRoot + FRegSubKey;
end;

procedure TWCCLoginDlg.SetAllowOSAuthentication(const Value: boolean);
begin
  FAllowOSAuthentication := Value;
end;

procedure TWCCLoginDlg.ShowConnectionProperties;
begin
  with TFConnectionProperties.Create(Owner,FADOConnection) do try
    ShowModal;
  finally
    Free;
  end;
end;

//{ TSecondaryConnection }
//
//procedure TSecondaryConnection.SetEnabled(const Value: boolean);
//begin
//  FEnabled := Value;
//end;
//
//procedure TSecondaryConnection.SetDB(const Value: TWCCConnection);
//begin
//  FDB := Value;
//end;
//
//constructor TSecondaryConnection.Create(Collection: TCollection);
//begin
//  inherited Create(Collection);
//  FEnabled := True;
//end;
//
//procedure TSecondaryConnection.Assign(Source: TPersistent);
//begin
//  if Source is TSecondaryConnection then
//  begin
//    DB      := TSecondaryConnection(Source).DB;
//    Enabled := TSecondaryConnection(Source).Enabled;
//  end
//  else inherited Assign(Source);
//end;
//
//function TSecondaryConnection.GetDisplayName: string;
//begin
//  if FDB <> nil then
//    Result := FDB.Name
//  else
//    Result := inherited GetDisplayName;
//end;

//{ TSecondaryConnections }
//
//constructor TSecondaryConnections.Create(LoginDlg: TWCCLoginDlg);
//begin
//  inherited Create(TSecondaryConnection);
//  FWCCLoginDlg := LoginDlg;
//end;
//
//function TSecondaryConnections.Add: TSecondaryConnection;
//begin
//  Result := TSecondaryConnection(inherited Add);
//end;
//
//function TSecondaryConnections.GetItem(Index: Integer): TSecondaryConnection;
//begin
//  Result := TSecondaryConnection(inherited GetItem(Index));
//end;
//
//procedure TSecondaryConnections.SetItem(Index: Integer;
//  const Value: TSecondaryConnection);
//begin
//  inherited SetItem(Index, Value);
//end;
//
//function TSecondaryConnections.AddItem(Item: TSecondaryConnection;
//  Index: Integer): TSecondaryConnection;
//begin
//  if Item = nil then
//    Result := TSecondaryConnection.Create(Self)
//  else
//    Result := Item;
//  if Assigned(Result) then
//  begin
//    Result.Collection := Self;
//    if Index < 0 then
//      Index := Count - 1;
//    Result.Index := Index;
//  end;
//end;
//
//procedure TSecondaryConnections.Update(Item: TCollectionItem);
//begin
//  inherited;
//  { do nothing? }
//end;
//
//function TSecondaryConnections.Insert(Index: Integer): TSecondaryConnection;
//begin
//  Result := AddItem(nil, Index);
//end;
//
//function TSecondaryConnections.GetOwner: TPersistent;
//begin
//  Result := FWCCLoginDlg;
//end;
//
//procedure TWCCLoginDlg.SetSecondaryConnections(
//  const Value: TSecondaryConnections);
//begin
//  FSecondaryConnections := Value;
//end;

end.
