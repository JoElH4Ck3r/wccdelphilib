unit classTMReports;

interface

uses classes, sysutils, DB, ADODB;

type
  ECustomReportException = Exception;

  TReportInfo = class
  private
    FCustom: boolean;
    FID: integer;
    FType: string;
    FPath: string;
    FName: string;
    FStatus: string;
    FDescription: string;
    FChanged: boolean;
    function GetReportName: string;
  public
    constructor Create(const AID : integer; const AType, APath, AName, AStatus, ADescription : string; ACustom : boolean);
    constructor CreateNew;
    property ID : integer read FID write FID;
    property _Type : string read FType write FType;
    property Path : string read FPath write FPath;
    property Name : string read FName write FName;
    property Status : string read FStatus write FStatus;
    property Custom : boolean read FCustom write FCustom;
    property Description : string read FDescription write FDescription;
    { Non database members }
    property Changed : boolean read FChanged write FChanged;
    property ReportName : string read GetReportName;
  end;

  TReportList = class(TList)
  private
    function GetReports(index: integer): TReportInfo;
    procedure SetReports(index: integer; const Value: TReportInfo);
  public
    procedure Clear; override;
    function AddReport(AReport : TReportInfo):integer;
    property Reports[index : integer]:TReportInfo read GetReports write SetReports; default;
  end;

  TCustomTMReports = class
  private
    FDB : TADOConnection;
    qReportsByType :TADOQuery;
    qReportsType :TADOQuery;
    qReports :TADOQuery;
    qUpdateReport :TADOQuery;
    qAddReport :TADOQuery;
    qDeleteReport :TADOQuery;
    FPrepared : boolean;
    FProgName: string;
    procedure AssertDBConnection;
  protected
    procedure PrepareQueries;
    property ProgName : string read FProgName;
  public
    constructor Create(ADB : TADOConnection); reintroduce;
    destructor Destroy; override;
    procedure ReportsByType(const ReportType : string; ReportList : TReportList);
    procedure Reports(ReportList : TReportList);
    procedure ReportTypes(ReportTypeList : TStringList);
    procedure UpdateReport(AReport : TReportInfo);
    procedure AddReport(AReport : TReportInfo);
    procedure DeleteReport(AReport : TReportInfo);
  end;

  TTMReports = class(TCustomTMReports)
  end;

function NullStr(const s : string):string;

implementation

uses WCCUtil;

resourcestring
  ErrConnection = 'Database not connected';

function NullStr(const s : string):string;
begin
  Result := s;
  if Length(s) = 0 then
    Result := #0;
end;

{ TCustomTMReports }

procedure TCustomTMReports.AssertDBConnection;
begin
  if not FDB.Connected then
    raise ECustomReportException.Create(ErrConnection);
  if not FPrepared then
    PrepareQueries;
end;

constructor TCustomTMReports.Create(ADB: TADOConnection);
begin
  inherited Create;
  FDB := ADB;
  qReportsByType := TADOQuery.Create(nil);
  qReportsType := TADOQuery.Create(nil);
  qReports := TADOQuery.Create(nil);
  qUpdateReport := TADOQuery.Create(nil);
  qAddReport := TADOQuery.Create(nil);
  qDeleteReport := TADOQuery.Create(nil);
  FPrepared := False;
end;

destructor TCustomTMReports.Destroy;
begin
  qDeleteReport.Free;
  qUpdateReport.Free;
  qAddReport.Free;
  qReports.Free;
  qReportsType.Free;
  qReportsByType.Free;
  inherited;
end;

procedure TCustomTMReports.PrepareQueries;
begin
  with qReportsByType do begin
    Connection := FDB;
    SQL.Add('SELECT RPT_ID,RPT_TYPE,RPT_PATH,RPT_NAME,RPT_STATUS,RPT_CUSTOM,RPT_DESCRIPTION ');
    SQL.Add('FROM USER_REPORTS ');
    SQL.Add('WHERE RPT_TYPE = :RPT_TYPE ');
    SQL.Add('WITH UR ');
  end;
  with qReports do begin
    Connection := FDB;
    SQL.Add('SELECT RPT_ID,RPT_TYPE,RPT_PATH,RPT_NAME,RPT_STATUS,RPT_CUSTOM,RPT_DESCRIPTION ');
    SQL.Add('FROM USER_REPORTS ');
    SQL.Add('WITH UR ');
  end;
  with qReportsType do begin
    Connection := FDB;
    SQL.Add('SELECT RPT_TYPE FROM USER_REPORTS GROUP BY RPT_TYPE ORDER BY 1 WITH UR');
  end;
  with qUpdateReport do begin
    Connection := FDB;
    SQL.Add('UPDATE USER_REPORTS SET ');
    SQL.Add('RPT_TYPE = :RPT_TYPE, ');
    SQL.Add('RPT_PATH = :RPT_PATH, ');
    SQL.Add('RPT_NAME = :RPT_NAME, ');
    SQL.Add('RPT_CUSTOM = :RPT_CUSTOM, ');
    SQL.Add('RPT_DESCRIPTION = :RPT_DESCRIPTION ');
    SQL.Add('WHERE RPT_ID = :RPT_ID');
  end;
  with qAddReport do begin
    Connection := FDB;
    SQL.Add('INSERT INTO USER_REPORTS(RPT_ID,RPT_TYPE,RPT_PATH,RPT_NAME,RPT_CUSTOM,RPT_DESCRIPTION)');
    SQL.Add('VALUES(NEXTVAL FOR GEN_REPORTS_ID,:RPT_TYPE,:RPT_PATH,:RPT_NAME,:RPT_CUSTOM,:RPT_DESCRIPTION)');
    Prepared := True;
  end;
  with qDeleteReport do begin
    Connection := FDB;
    SQL.Add('DELETE FROM USER_REPORTS WHERE RPT_ID = :RPT_ID');
  end;
  FPrepared := True;
end;

procedure TCustomTMReports.Reports(ReportList: TReportList);
var
  RI : TReportInfo;
begin
  if ReportList = nil then
    raise Exception.Create('Invalid Report List');
  AssertDBConnection;
  ReportList.Clear;
  with qReports do begin
    if Active then Close;
    try
      Open;
      while not Eof do begin
        RI := TReportInfo.Create(
                FieldByName('RPT_ID').AsInteger,
                FieldByName('RPT_TYPE').AsString,
                FieldByName('RPT_PATH').AsString,
                FieldByName('RPT_NAME').AsString,
                FieldByName('RPT_STATUS').AsString,
                FieldByName('RPT_DESCRIPTION').AsString,
                FieldByName('RPT_CUSTOM').AsString='True'
                );
        ReportList.AddReport(RI);
        Next;
      end;
    finally
      Close;
      if Assigned(RI) then RI.Free;
    end;
  end;
end;

procedure TCustomTMReports.ReportsByType(const ReportType: string;
  ReportList: TReportList);
var
  RI : TReportInfo;
  s : string;
begin
  if ReportList = nil then
    raise Exception.Create('Invalid Report List');
  if Length(ReportType) = 0 then
    Reports(ReportList)
  else
  begin
    AssertDBConnection;
    ReportList.Clear;
    with qReportsByType do begin
      if Active then Close;
      Parameters.ParamByName('RPT_TYPE').Value := ReportType;
      try
        Open;
        while not Eof do begin
          s := Trim(FieldByName('RPT_CUSTOM').AsString);
          RI := TReportInfo.Create(
                  FieldByName('RPT_ID').AsInteger,
                  FieldByName('RPT_TYPE').AsString,
                  FieldByName('RPT_PATH').AsString,
                  FieldByName('RPT_NAME').AsString,
                  FieldByName('RPT_STATUS').AsString,
                  FieldByName('RPT_DESCRIPTION').AsString,
                  s='True'
                  );
          ReportList.AddReport(RI);
          Next;
        end;
      finally
        Close;
        if Assigned(RI) then RI.Free;
      end;
    end;
  end;
end;

procedure TCustomTMReports.ReportTypes(ReportTypeList: TStringList);
begin
  if ReportTypeList = nil then
    raise Exception.Create('Invalid report type list');
  AssertDBConnection;
  ReportTypeList.Clear;
  with qReportsType do begin
    if Active then Close;
    try
      Open;
      while not Eof do begin
        ReportTypeList.Add(Fields[0].AsString);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TCustomTMReports.AddReport(AReport: TReportInfo);
begin
  if AReport = nil then
    raise Exception.Create('Invalid Report List');
  AssertDBConnection;
  with qAddReport do begin
    //Parameters.ParamByName('RPT_ID').Value := 0;
    Parameters.ParamByName('RPT_TYPE').Value := NullStr(AReport._Type);
    Parameters.ParamByName('RPT_PATH').Value := NullStr(AReport.Path);
    Parameters.ParamByName('RPT_NAME').Value := NullStr(AReport.Name);
    Parameters.ParamByName('RPT_CUSTOM').Value := iif_str(AReport.Custom,'True','False');
    Parameters.ParamByName('RPT_DESCRIPTION').Value := NullStr(AReport.Description);
    ExecSQL;
  end;
end;

procedure TCustomTMReports.UpdateReport(AReport: TReportInfo);
begin
  if AReport = nil then
    raise Exception.Create('Invalid Report List');
  AssertDBConnection;
  with qUpdateReport do begin
    Parameters.ParamByName('RPT_ID').Value := AReport.ID;
    Parameters.ParamByName('RPT_TYPE').Value := NullStr(AReport._Type);
    Parameters.ParamByName('RPT_PATH').Value := NullStr(AReport.Path);
    Parameters.ParamByName('RPT_NAME').Value := NullStr(AReport.Name);
    Parameters.ParamByName('RPT_CUSTOM').Value := iif_str(AReport.Custom,'True','False');
    Parameters.ParamByName('RPT_DESCRIPTION').Value := NullStr(AReport.Description);
    ExecSQL;
  end;
end;

procedure TCustomTMReports.DeleteReport(AReport: TReportInfo);
begin
  if AReport = nil then
    raise Exception.Create('Invalid Report List');
  AssertDBConnection;
  with qDeleteReport do begin
    Parameters.ParamByName('RPT_ID').Value := AReport.ID;
    ExecSQL;
  end;
end;

{ TReportInfo }

constructor TReportInfo.Create(const AID : integer; const AType, APath, AName,
  AStatus, ADescription : string; ACustom : boolean);
begin
  FChanged := False;
  FCustom := ACustom;
  FID := AID;
  FType := AType;
  FPath := APath;
  FName := AName;
  FStatus := AStatus;
  FDescription := ADescription;
end;

constructor TReportInfo.CreateNew;
begin
  FChanged := False;
  FCustom := True;
  FID := -1;
  FType := '';
  FPath := '';
  FName := '';
  FStatus := '';
  FDescription := '';
end;

function TReportInfo.GetReportName: string;
begin
  Result := FPath + FName;
end;

{ TReportList }

function TReportList.AddReport(AReport: TReportInfo):integer;
begin
  if AReport = nil then
    raise Exception.Create('Invalid Report Object');
  with AReport do
    Result := Add(TReportInfo.Create(ID,_Type,Path,Name,Status,Description,Custom));
end;

procedure TReportList.Clear;
var
  i : integer;
begin
  for i := 0 to Count -1 do TReportInfo(Items[i]).Free;
  inherited;
end;

function TReportList.GetReports(index: integer): TReportInfo;
begin
  Result := TReportInfo(Items[index]);
end;

procedure TReportList.SetReports(index: integer; const Value: TReportInfo);
begin
  Items[index] := Value;
end;

end.
