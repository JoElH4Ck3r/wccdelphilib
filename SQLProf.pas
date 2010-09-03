unit SQLProf;

interface

uses
  Classes, DB, ADODB, SysUtils, Windows;

type
  TSQLCommandType = (ctOpen, ctExecute);

  PParamData = ^TParamData;
  TParamData = record
    Name : string[40];
    Data : Variant;
  end;

  TProfileEntry = class(TPersistent)
  private
    FDuration: DWord;
    FCommandType: TSQLCommandType;
    FSQL: string;
    FName: string;
    FStatementTime: TDateTime;
    FParamData : TList;
    function GetParamCount: integer;
  public
    constructor Create(const ADuration : DWord; const AName, ASQL : string;
      const ACommandType : TSQLCommandType; Params : TParameters); virtual;
    destructor Destroy; override;
    property StatementTime : TDateTime read FStatementTime;
    property Duration : DWord read FDuration;
    property SQL : string read FSQL write FSQL;
    property QueryName : string read FName write FName;
    property CommandType : TSQLCommandType read FCommandType write FCommandType;
    property ParamCount : integer read GetParamCount;
    property ParamData : TList read FParamData;
  end;

  TProfileEntries = class(TList)
  private
    function GetProfileEntry(index: integer): TProfileEntry;
  public
    procedure Clear;override;
    property Entry [index : integer]: TProfileEntry read GetProfileEntry; default;
  end;

  TProfileCallBack = procedure(const QueryName, SQL : string;
    const ADuration : DWord; Params : TParameters) of object;

  TDatasetProfiler = class
  private
    FSQL : string;
    FQueryName : string;
    FStartTime : DWord;//TDateTime;
    FDataset : TDataSet;
    FProfileCallback : TProfileCallBack;
    FOldBeforeOpen: TDataSetNotifyEvent;
    FOldAfterOpen: TDataSetNotifyEvent;
  public
    constructor Create(ADS : TDataSet; ProfileCallback : TProfileCallBack);
    destructor Destroy; override;
    {}
    procedure BeforeOpen(DataSet: TDataSet);
    procedure AfterOpen(DataSet: TDataSet);
  end;

  TSQLProfiler = class(TPersistent)
  private
    FConnection : TCustomConnection;
    FProfileEntries : TProfileEntries;
    FActive: boolean;
    FConnections : TList;
    procedure SetActive(const Value: boolean);
    function AddProfile(const QueryName, SQL : string; const ADuration : DWord;
      const ACommandType : TSQLCommandType; P : TParameters):integer;
    function ProfileExecSQL(ADataset: TCustomADODataSet): integer;
  protected
    procedure ProfileCallback(const QueryName, SQL : string;
      const ADuration : DWord; Params : TParameters);
  public
    constructor Create(DBConnection : TCustomConnection);
    destructor Destroy; override;
    procedure Save(const Filename : string);
    procedure RegisterConnection(DBConnection: TCustomConnection);
    property Active : boolean read FActive write SetActive;
    procedure Clear;
    //function ProfileExecSQL(ADataset : TDataset):integer;
    function ProfileADOExec(ADataset : TADOQuery):integer;
  end;

//var
//  SQLProfiler : TSQLProfile = nil;

implementation

uses Variants;

{ TProfileEntries }

function TProfileEntries.GetProfileEntry(index: integer): TProfileEntry;
begin
  Result := TProfileEntry(Items[index]);
end;

procedure TProfileEntries.Clear;
var
  i : integer;
begin
  for i := 0 to COunt - 1 do
    TProfileEntry(Items[i]).Free;
  inherited;
end;

{ TSQLProfiler }

constructor TSQLProfiler.Create(DBConnection: TCustomConnection);
var
  i : integer;
begin
  inherited Create;
  FProfileEntries := TProfileEntries.Create;
  FConnections := TList.Create;
  if DBConnection = nil then
    raise Exception.Create('Not a valid connection object');
  FConnection := DBConnection;
  for i := 0 to DBConnection.DataSetCount - 1 do
    FConnections.Add(TDatasetProfiler.Create(DBConnection.DataSets[i],ProfileCallback));
end;

destructor TSQLProfiler.Destroy;
var
  i : integer;
begin
  for i := 0 to FConnections.Count - 1 do
    TDatasetProfiler(FConnections.Items[i]).Free;
  FConnections.Free;
  FProfileEntries.Free;
  inherited;
end;

procedure TSQLProfiler.Save(const Filename: string);
const
  CRLF = #13+#10;
var
  FS : TFileStream;
  i, j : integer;
  s, s1, sHeader : string;
begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  try
    for i := 0 to FProfileEntries.Count - 1 do begin
      sHeader := CRLF+'*********************************************************'+CRLF;
      sHeader := sHeader + Format('Time: %s',[FormatDateTime('hh:nn:ss.zzz',FProfileEntries[i].StatementTime)]) + CRLF;
      sHeader := sHeader + Format('Name: %s',[FProfileEntries[i].QueryName]) + CRLF;
      sHeader := sHeader + Format('Duration: %d', [FProfileEntries[i].Duration]) + CRLF;
      sHeader := sHeader + 'Parameters:' + CRLF;
      FS.Write(sHeader[1],Length(sHeader));

      if FProfileEntries[i].ParamCount > 0 then begin
        for j := 0 to FProfileEntries[i].ParamData.Count - 1 do begin
          s1 := Format('  %s = %s'+#13+#10,[
                  PParamData(FProfileEntries[i].ParamData[j])^.Name,
                  VarToStr(PParamData(FProfileEntries[i].ParamData[j])^.Data)
                  ]);
          FS.Write(s1[1],Length(s1));
        end;
      end;
      s := CRLF + FProfileEntries[i].SQL + CRLF;
//      s := Format('"%s","%d","%s","%s"'+#13+#10,[
//             FormatDateTime('hh:nn:ss.zzz',FProfileEntries[i].StatementTime),
//             FProfileEntries[i].Duration,
//             FProfileEntries[i].QueryName,
//             FProfileEntries[i].SQL
//             ]);
      FS.Write(s[1],Length(s));
    end
  finally
    FS.Free;
  end;
end;

procedure TSQLProfiler.Clear;
begin
  FProfileEntries.Clear;
end;

procedure TSQLProfiler.RegisterConnection(DBConnection: TCustomConnection);
begin

end;

procedure TSQLProfiler.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

//function TSQLProfiler.ProfileExecSQL(ADataset: TDataset): integer;
//var
//  Start : TDateTime;
//  s : string;
//begin
//  if FActive then
//    Start := Now;
//
//  try
////    if ADataset is TADOQuery then
////      s := (ADataset is TADOQuery). SQL;
//    Result := ADataset.ExecSQL;
//  finally
//    if FActive then
//      AddProfile(s, Start, Now, ctExecute);
//  end
//end;

function TSQLProfiler.ProfileADOExec(ADataset: TADOQuery): integer;
var
  Start : DWord;
  s : string;
begin
  if FActive then
    Start := GetTickCount;

  try
    s := ADataset.SQL.Text;
    Result := ADataset.ExecSQL;
  finally
    if FActive then
      AddProfile(ADataset.Name,  s, GetTickCount - Start, ctExecute, ADataset.Parameters);
  end;
end;

procedure TSQLProfiler.ProfileCallback(const QueryName, SQL: string;
  const ADuration : DWord; Params : TParameters);
begin
  if FActive then
    AddProfile(QueryName, SQL, ADuration, ctOpen, Params);
end;

function TSQLProfiler.AddProfile(const QueryName, SQL: string;
  const ADuration : DWord; const ACommandType: TSQLCommandType;
  P : TParameters):integer;
begin
  Result := FProfileEntries.Add(
              TProfileEntry.Create(
                ADuration,
                QueryName,
                SQL,
                ACommandType,
                P
                ));
end;

function TSQLProfiler.ProfileExecSQL(ADataset: TCustomADODataSet): integer;
begin
  Result := 0;
end;

{ TProfileEntry }

//constructor TProfileEntry.Create(const AStartTime, AEndTime: TDateTime;
//  const AName, ASQL: string; const ACommandType: TSQLCommandType);
constructor TProfileEntry.Create(const ADuration : DWord;
  const AName, ASQL : string; const ACommandType : TSQLCommandType;
  Params : TParameters);
var
  i : integer;
  P : PParamData;
begin
  inherited Create;
  FParamData := TList.Create;
  for i := 0 to Params.Count - 1 do begin
    New(P);
    P^.Name := Params[i].Name;
    P^.Data := Params[i].Value;
    FParamData.Add(P);
  end;
  FStatementTime := Now;
  FDuration := ADuration;
  FCommandType := ACommandType;
  FSQL := ASQL;
  FName := AName;
end;

function TProfileEntry.GetParamCount: integer;
begin
  Result := FParamData.Count;
end;

destructor TProfileEntry.Destroy;
var
  i : integer;
begin
  for i := 0 to FParamData.Count - 1 do
    Dispose(FParamData[i]);
  FParamData.Free;
  inherited;
end;

{ TDatasetProfiler }

constructor TDatasetProfiler.Create(ADS: TDataSet;
  ProfileCallback: TProfileCallBack);
begin
  inherited Create;
  FProfileCallback := ProfileCallback;
  FDataset := ADS;
  FQueryName := FDataset.Name;
  if ADS is TADOQuery then
    FSQL := (ADS as TADOQuery).SQL.Text;

  { Save }
  FOldBeforeOpen := FDataset.BeforeOpen;
  FOldAfterOpen := FDataset.AfterOpen;
  { Assign }
  FDataset.BeforeOpen := BeforeOpen;
  FDataset.AfterOpen := AfterOpen;
end;

//procedure TDatasetProfiler.AfterOpen(DataSet: TCustomADODataSet);
procedure TDatasetProfiler.AfterOpen(DataSet: TDataSet);
begin
  FProfileCallback(FQueryName, FSQL, GetTickCount - FStartTime,
    TADOQuery(DataSet).Parameters);
  if Assigned(FOldAfterOpen) then
    FOldAfterOpen(FDataset);
end;

procedure TDatasetProfiler.BeforeOpen(DataSet: TDataSet);
begin
  if Assigned(FOldBeforeOpen) then
    FOldBeforeOpen(FDataset);
  FStartTime := GetTickCount;
end;

destructor TDatasetProfiler.Destroy;
begin
  FDataset.BeforeOpen := FOldBeforeOpen;
  FDataset.AfterOpen := FOldAfterOpen;
  inherited;
end;

//initialization
//  SQLProfiler := TSQLProfile.Create;
//
//finalization
//  SQLProfiler.Free;

end.
