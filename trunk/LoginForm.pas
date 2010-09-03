unit LoginForm;

interface

{$I WCC.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, Registry, ADODB, WestCoastCode,
  JvExControls, JvComponent, JvProgressBar,
  CommandLineSwitches, Buttons, LoginOptions, JvComponentBase, JvBalloonHint;

const
  WM_AFTER_SHOW = WM_USER + 300; // custom message

type
  EDBLoginDlgError = class(Exception);

  TLoginFailureEvent = procedure(Sender : TObject;const ErrorMessage : string) of object;

  TLoginProgressThread = class(TThread)
  private
    FProgressMethod : TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(AProgressMethod : TThreadMethod);
  end;

  TConnectProc = function(const U, P, D, S, F: string):boolean of object;

  TFDBLogin = class(TForm)
    panelForm: TPanel;
    Label3: TLabel;
    bOK: TButton;
    bCancel: TButton;
    cbDSN: TComboBox;
    panelSeparator: TPanel;
    panelHeader: TPanel;
    Label6: TLabel;
    Image1: TImage;
    LoginProgress: TJvGradientProgressBar;
    Label1: TLabel;
    eUsername: TEdit;
    Label2: TLabel;
    ePassword: TEdit;
    cbWindowsAuthentication: TCheckBox;
    bAdvancedOptions: TSpeedButton;
    pbGradient: TPaintBox;
    HintOptions: TJvBalloonHint;
    procedure cbDSNChange(Sender: TObject);
    procedure cbWindowsAuthenticationClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure panelFormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure panelFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure panelFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure bAdvancedOptionsClick(Sender: TObject);
  private
    FX, FY : integer;
    FMouseDown : boolean;
    FInLogin : integer;
//    FProgressThread : TLoginProgressThread;
    FConnectProc : TConnectProc;

    FLoginOptionsData : TLoginOptionsData;

    function GetDatabase: string;
    function GetPassword: string;
    function GetUsername: string;
    function GetInLogin: boolean;
    {}
    //procedure StepLoginProgress;
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    function GetSchema: string;
    procedure DoLoginAdvancedOptionsHint;
    function GetPath: string;
  public
    constructor Create(AOwner : TComponent; ADSNList : TStrings;
      AConnectProc : TConnectProc); reintroduce;
    destructor Destroy; override;
    procedure SetSaveValues(const ADB, AUsername, ASchema, APath : string);
    {}
    procedure BeginLogin;
    procedure EndLogin;
    property InLogin : boolean read GetInLogin;
    {}
    property UserName : string read GetUsername;
    property Password : string read GetPassword;
    property Database : string read GetDatabase;
    property Schema : string read GetSchema;
    property Path : string read GetPath;
  end;

var
  FDBLogin: TFDBLogin;

implementation

uses ODBC, JclStrings, WCCUtil, LoginConst, LoginDlg;

{$R *.dfm}

{ TFDBLogin }

constructor TFDBLogin.Create(AOwner: TComponent; ADSNList : TStrings;
  AConnectProc : TConnectProc);
begin
  inherited Create(AOwner);
  if ADSNList = nil then
    raise Exception.Create(ERR_MSG_INVALID_DSN_LIST);
  {}
  FLoginOptionsData := TLoginOptionsData.Create;

  cbDSN.Items.Assign(ADSNList);
  FConnectProc := AConnectProc;

  if (AOwner is TWCCLoginDlg) then begin
    with (AOwner as TWCCLoginDlg) do begin
      cbWindowsAuthentication.Enabled := AllowOSAuthentication;
      cbWindowsAuthentication.Checked := AllowOSAuthentication;
      FLoginOptionsData.Schema := Schema;
    end;
  end;

//  FProgressThread := TLoginProgressThread.Create(StepLoginProgress);

  FInLogin := 0;
  LoginProgress.Visible := False;

  ePassword.PasswordChar := PasswordCharacter;

  DrawGradient(clSkyBlue, clWhite, pbGradient.Canvas, pbGradient.ClientRect);

end;

destructor TFDBLogin.Destroy;
begin
//  FProgressThread.Terminate;
  FLoginOptionsData.Free;
  inherited;
end;

procedure TFDBLogin.FormShow(Sender: TObject);
begin
////  eUserName.Text := '';
//  if Length(eUserName.Text) > 0 then begin
//    ePassword.Text := '';
////    if Visible then
////      ePassword.SetFocus();
//  end;
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TFDBLogin.WMAfterShow(var Msg: TMessage);
begin
  if Length(eUserName.Text) > 0 then begin
    ePassword.Text := '';
//    if Visible then
//      ePassword.SetFocus();
    ePassword.SetFocus();
  end else
    eUserName.SetFocus;

  DoLoginAdvancedOptionsHint;
end;

procedure TFDBLogin.DoLoginAdvancedOptionsHint;
begin
//  if Length(FLoginOptionsData.Schema) = 0 then begin
//    HintOptions.ActivateHint(
//      bAdvancedOptions,
//      'The database schema name has not been set.'+#13+'This is required when connecting to a TruckMate™ database',
//      ikWarning,
//      'Connection Options',
//      10000
//      );
//  end;
end;

function TFDBLogin.GetDatabase: string;
begin
  with cbDSN do
    Result := Items[ItemIndex];
end;

function TFDBLogin.GetPassword: string;
begin
  Result := '';
  if not cbWindowsAuthentication.Checked then
    Result := ePassword.Text;
end;

function TFDBLogin.GetPath: string;
begin
  Result := FLoginOptionsData.Path;
end;

function TFDBLogin.GetSchema: string;
begin
  Result := FLoginOptionsData.Schema;
end;

function TFDBLogin.GetUsername: string;
begin
  Result := '';
  if not cbWindowsAuthentication.Checked then
    Result := eUserName.Text;
end;

procedure TFDBLogin.SetSaveValues(const ADB, AUsername, ASchema, APath: string);
begin
  eUserName.Text := AUsername;
  with cbDSN do
    if Items.IndexOf(ADB) > -1 then
      ItemIndex := Items.IndexOf(ADB);
  FLoginOptionsData.Schema := ASchema;
  FLoginOptionsData.Path := APath;
end;

procedure TFDBLogin.bAdvancedOptionsClick(Sender: TObject);
begin
//  InfoMsg('Login Advanced Options');
  with TFLoginOptions.Create(Self, FLoginOptionsData) do try
    if ShowModal = mrOK then
      FLoginOptionsData.Assign(LoginOptionsData);
      //InfoMsg('Change Settings');
  finally
    Free;
  end;
end;

function TFDBLogin.GetInLogin: boolean;
begin
  Result := FInLogin > 0;
end;

//procedure TFDBLogin.StepLoginProgress;
//begin
//  with LoginProgress do
//    if Position + 5 > Max then
//      Position := 0
//    else
//      Position := Position + 5;
//  Application.ProcessMessages;
//end;

procedure TFDBLogin.BeginLogin;
begin
  Inc(FInLogin);

//  LoginProgress.Visible := True;
//  FProgressThread := TLoginProgressThread.Create(StepLoginProgress);
//  FProgressThread.Resume;
end;

procedure TFDBLogin.EndLogin;
begin
  Dec(FInLogin);

//  if FProgressThread <> nil then FProgressThread.Suspend;
//  LoginProgress.Visible := False;
end;

procedure TFDBLogin.panelFormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FX := X;
  FY := Y;
end;

procedure TFDBLogin.panelFormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FMouseDown then begin
    Left := Left + (X - FX);
    Top := Top + (Y - FY);
  end;
end;

procedure TFDBLogin.panelFormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;

procedure TFDBLogin.bOKClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    BeginLogin;
    try
      if FConnectProc(Username, Password, Database, Schema, Path) then ModalResult := mrOK
      else ModalResult := mrNone;
    finally
      EndLogin;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

{ TLoginProgressThread }

constructor TLoginProgressThread.Create(AProgressMethod: TThreadMethod);
begin
  inherited Create(True);
  FProgressMethod := AProgressMethod;
  FreeOnTerminate := True;
  //Resume;
end;

procedure TLoginProgressThread.Execute;
begin
  while not Terminated do begin
    Sleep(100);
    Synchronize(FProgressMethod);
//    WCCTrace('TLoginProgressThread.Execute')
  end;
end;

procedure TFDBLogin.cbWindowsAuthenticationClick(Sender: TObject);
begin
  eUserName.Enabled := not cbWindowsAuthentication.Checked;
  ePassword.Enabled := not cbWindowsAuthentication.Checked;
end;

procedure TFDBLogin.cbDSNChange(Sender: TObject);
begin
  {}
end;

end.
