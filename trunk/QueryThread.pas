unit QueryThread;

interface
(*
TODO - Handle Integer searches
---------------------------
Error
---------------------------
[IBM][CLI Driver][DB2/NT] SQL0440N  No function by the name "LIKE" having
compatible arguments was found in the function path.  SQLSTATE=42884
---------------------------
OK
---------------------------
*)

uses
  Classes, ADODB, ADOINT, Sysutils, DBConnection, SQLTypes, DB, ActiveX, Windows;

const
  MAX_SORT_COL = 5;

type
//  TThreadError = procedure(const ErrorMessage : string) of object;
//  TThreadMessage = procedure(const Msg : string) of object;

  TFetchOutcome = (foEof, foChangeInterupt, foException);

//  TBaseADOQueryThread = class(TThread)
//  private
//    FLastErrorMessage : string;
//    FDatabase: string;
//    FPassword: string;
//    FUsername: string;
//    FDB : TWCCConnection;
//    FQuery : TADOQuery;
//    FOnError: TThreadError;
//    FDebugMessage, FMessage : string;
//    FDebug: boolean;
//    FOnMessage: TThreadMessage;
//    UpdateCriticalSection: TRTLCriticalSection;
//    SortCriticalSection: TRTLCriticalSection;
//    procedure SetLastErrorMessage(const Value: string);
//    procedure SetOnError(const Value: TThreadError);
//    procedure DoSyncDebugMessage;
//    procedure DoSyncMessage;
//    procedure SyncDebugMessage(const Msg : string);
//    procedure SyncMessage(const Msg : string);
//    procedure SetDebug(const Value: boolean);
//    procedure SetOnMessage(const Value: TThreadMessage);
//  protected
//    procedure Execute; override;
//    procedure DoError;
//    function GetWhereClause:string; virtual;
//    procedure OpenQuery;
//    procedure CloseQuery;
//  public
//    constructor Create(const U, P, D : string); virtual;
//    destructor Destroy; override;
//    property LastErrorMessage : string read FLastErrorMessage write SetLastErrorMessage;
//    property OnError : TThreadError read FOnError write SetOnError;
//    property OnMessage : TThreadMessage read FOnMessage write SetOnMessage;
//    property Debug : boolean read FDebug write SetDebug;
//    { Move Termninated to public visibility }
//    property Terminated;
//  end;

//  TSQLCountEvent = procedure(const TableName : string; const RecordCount : integer) of object;

//  TSQLCountThread = class(TBaseADOQueryThread)
//  private
//    FTableName : string;
//    FCount : integer;
//    FSQLCountEvent : TSQLCountEvent;
//  protected
//    procedure DoCount; virtual;
//    procedure ExecuteCount;
//    procedure Execute; override;
//  public
//    constructor Create(const U, P, D, ATableName : string;
//      CountEvent : TSQLCountEvent); reintroduce; virtual;
//  end;

  TFieldInfo = class(TPersistent)
  private
    FLength: integer;
    FName: string;
    FFieldType: TFieldType;
    FFieldTypeName: string;
    FScale: integer;
    FFilterValue: Variant;
    FVisible: boolean;
    FFiltered: boolean;
    FWherePredicate: TWherePredicate;
    FKeySequence: integer;
    procedure SetFieldType(const Value: TFieldType);
    procedure SetFieldTypeName(const Value: string);
    procedure SetLength(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetScale(const Value: integer);
    procedure SetFiltered(const Value: boolean);
    procedure SetFilterValue(const Value: Variant);
    procedure SetVisible(const Value: boolean);
    procedure SetWherePredicate(const Value: TWherePredicate);
    procedure SetKeySequence(const Value: integer);
  public
    constructor Create(const AName, ATypeName : string; const AScale, ALength : integer); overload;
    constructor Create(const AName : string); overload;
    constructor Create(const AName : string; AVisible, AFilter : boolean;
      AFilterValue : Variant; Predicate : TWherePredicate); overload;
    property Name : string read FName write SetName;
    property FieldType : TFieldType read FFieldType write SetFieldType;
    property FieldTypeName : string read FFieldTypeName write SetFieldTypeName;
    property Scale : integer read FScale write SetScale;
    property Length : integer read FLength write SetLength;
    property Visible : boolean read FVisible write SetVisible;
    property Filtered : boolean read FFiltered write SetFiltered;
    property FilterValue : Variant read FFilterValue write SetFilterValue;
    property WherePredicate : TWherePredicate read FWherePredicate write SetWherePredicate;
    property KeySequence : integer read FKeySequence write SetKeySequence;
  end;

  TFieldInfoList = class(TList)
  private
    function GetFieldInfo(index: integer): TFieldInfo;
  public
    procedure Clear; override;
    property Fields [index : integer]: TFieldInfo read GetFieldInfo; default;
    function FindField(const FieldName : string):TFieldInfo;
  end;

  TRowData = class(TPersistent)
  private
    FData : array of Variant;
    FFieldCount: integer;
    function GetData(index: integer): Variant;
    procedure SetData(index: integer; const Value: Variant);
    procedure SetFieldCount(const Value: integer);
  public
    constructor Create(const AFieldCount : integer);
    procedure Assign(Source: TPersistent); override;
    property Data [index : integer] : Variant read GetData write SetData; default;
    function AsString(index : integer) : string;
    property FieldCount : integer read FFieldCount write SetFieldCount;
  end;

  PFieldQueryInfo = ^TFieldQueryInfo;
  TFieldQueryInfo = record
    FieldName       : string[128];
    Visible         : boolean;
    Filter          : boolean;
    FilterPredicate : TWherePredicate;
    FilterValue     : Variant;
  end;

  TSQLRowList = class(TList)
  private
    //FFields : TStringList;
    FFields : TFieldInfoList;
    FTableName: string;
    FReading: boolean;
    FRecordCount: integer;
    FOnAdd: TNotifyEvent;
    FUserFilterValue: Variant;
    FUserFilterField: string;
    FDisableSorting: boolean;
    procedure SetTableName(const Value: string);
    procedure SetReading(const Value: boolean);
    procedure SetRecordCount(const Value: integer);
    function GetRow(index: integer): TRowData;
    procedure SetRow(index: integer; const Value: TRowData);
    procedure SetOnAdd(const Value: TNotifyEvent);
    procedure SetUserFilterField(const Value: string);
    procedure SetUserFilterValue(const Value: Variant);
    function GetVisibleCount: integer;
    procedure SetDisableSorting(const Value: boolean);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const ATableName : string; const FieldNames : array of string); overload; virtual;
    constructor Create(const ATableName : string; const Fields : array of TFieldQueryInfo); overload; virtual;
    constructor Create(const ATableName : string); overload; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    function Clone : TSQLRowList;
    procedure SetFields(const Fields : array of TFieldQueryInfo);
    procedure AddRows(RowList : TSQLRowList);
    procedure SaveToFile(const FileName : string);
    property TableName : string read FTableName write SetTableName;
    property Reading : boolean read FReading write SetReading;
    property Fields : TFieldInfoList read FFields write FFields;
    property RecordCount : integer read FRecordCount write SetRecordCount;
    property Row [index : integer] : TRowData read GetRow write SetRow; default;
    property UserFilterField : string read FUserFilterField write SetUserFilterField;
    property UserFilterValue : Variant read FUserFilterValue write SetUserFilterValue;
    property VisibleCount : integer read GetVisibleCount;
    property DisableSorting : boolean read FDisableSorting write SetDisableSorting;
    { Events }
    property OnAdd : TNotifyEvent read FOnAdd write SetOnAdd;
  end;

//  TSQLQueryThread = class(TSQLCountThread) //class(TBaseADOQueryThread)
//  private
//    FRowList, FRowBuffer : TSQLRowList;
//    FFieldList : TFieldInfoList;
//    FBufferSize : integer;
//    {}
//    FSearchField : string;
//    FSearchValue : Variant;
//    FSearchCaseSensitive : boolean;
//    FSearchPredicidate : TWherePredicate;
//    FSearchChange, FHaveMetaData, FNeedParams : boolean;
//    {}
//    FCheckChangeThreshold : integer;
//    FSortStack : TSortStack;
//    procedure FlushBuffer;
//    function QueryFetch:TFetchOutcome;
//    function CheckForSearchCriteria: boolean;
//    function DoFetchQuery:boolean;
//    procedure SetQueryParameters;
//    procedure ClearRowList;
//  protected
//    procedure DoCount; override;
//    function ReadMetaData:boolean;
//    procedure Execute; override;
//    function FieldsString : string;
//    function WhereClause : string;
//  public
//    constructor Create(const U, P, D : string; ARowList : TSQLRowList;
//      const BufferSize : integer); reintroduce; virtual;
//    destructor Destroy; override;
//    procedure UpdateSearchCriteria(
//      const SearchField : string;
//      const SearchValue : Variant;
//      const SearchPredicate : TWherePredicate;
//      CaseSensitive : boolean = False
//      );
//    procedure AddSearchField(const FieldName : string);
//  end;


implementation

uses Dialogs, Variants, WCCUtil;

const
  DefCheckChangeThreshold = 500; // Twice per second
  ConStr = 'Provider=MSDASQL.1;Password=%s;Persist Security Info=True;User ID=%s;Data Source=%s';


  {$IFOPT D+}
  DefaultDebug : boolean = True;
  {$ELSE}
  DefaultDebug : boolean = False;
  {$ENDIF}

{ TBaseADOQueryThread }

//constructor TBaseADOQueryThread.Create(const U, P, D : string);
//begin
//  inherited Create(True);
//  { Initialize Variables }
//  FDebug := True;//DefaultDebug;
//  FLastErrorMessage := '';
//  FDatabase := D;
//  FPassword := P;
//  FUsername := U;
//
//  { Create Critical Sections for live syncronization }
//  InitializeCriticalSection(UpdateCriticalSection);
//  InitializeCriticalSection(SortCriticalSection);
//
//
//  { Setup the Database Connection }
//  FDB := TWCCConnection.Create(nil);
//  FDB.ConnectionString := Format(ConStr,[P,U,D]);
//
//  { Setup Generic Query - Shouldn't need more than one }
//  FQuery := TADOQuery.Create(nil);;
//  FQuery.Connection := FDB;
//end;

//destructor TBaseADOQueryThread.Destroy;
//begin
//  SyncDebugMessage('TBaseADOQueryThread.Destroy - Start');
//  //SyncDebugMessage('TBaseADOQueryThread.Destroy - Start');
//
//  { Shutdown Query }
//  if FQuery.Active then FQuery.Close;
//  FQuery.Free;
//
//  { Shutdown Database }
//  if FDB.Connected then FDB.Close;
//  FDB.Free;
//
//  { Unititialize COM }
//  CoUnInitialize;
//
//  { Delete CriticalSections }
//  DeleteCriticalSection(UpdateCriticalSection);
//  DeleteCriticalSection(SortCriticalSection);
//
//
//  //SyncDebugMessage('TBaseADOQueryThread.Destroy - End');
//  SyncDebugMessage('TBaseADOQueryThread.Destroy - End (Except for inherited)');
//  inherited;
//end;

//procedure TBaseADOQueryThread.DoError;
//begin
//  if Assigned(FOnError) then
//    FOnError(FLastErrorMessage);
//end;

//procedure TBaseADOQueryThread.DoSyncDebugMessage;
//begin
//  WCCTrace(FDebugMessage);
//end;

//procedure TBaseADOQueryThread.Execute;
//begin
//  SyncDebugMessage('TBaseADOQueryThread.Execute - Start');
//  SyncMessage('Initializing Database Connection...');
//  CoInitialize(nil);
//  { Override in base class }
//  if not Terminated then
//    try
//      FDB.Open;
//    except
//      on E : Exception do
//        LastErrorMessage := E.Message;
//    end;
//  SyncDebugMessage('TBaseADOQueryThread.Execute - End');
//end;

//procedure TBaseADOQueryThread.SetDebug(const Value: boolean);
//begin
//  FDebug := Value;
//end;

//procedure TBaseADOQueryThread.SetLastErrorMessage(const Value: string);
//begin
//  FLastErrorMessage := Value;
//  if Length(FLastErrorMessage) > 0 then
//    Synchronize(DoError);
//end;

//procedure TBaseADOQueryThread.SetOnError(const Value: TThreadError);
//begin
//  FOnError := Value;
//end;

//procedure TBaseADOQueryThread.SyncDebugMessage(const Msg: string);
//begin
////  if FDebug then begin
//    FDebugMessage := Msg;
//    Synchronize(DoSyncDebugMessage);
////  end;
//end;

//procedure TBaseADOQueryThread.SetOnMessage(const Value: TThreadMessage);
//begin
//  FOnMessage := Value;
//end;

//procedure TBaseADOQueryThread.DoSyncMessage;
//begin
//  if Assigned(FOnMessage) then
//    FOnMessage(FMessage);
//end;

//procedure TBaseADOQueryThread.SyncMessage(const Msg: string);
//begin
//  FMessage := Msg;
//  Synchronize(DoSyncMessage);
//end;

//function TBaseADOQueryThread.GetWhereClause: string;
//begin
//  {}
//end;

//procedure TBaseADOQueryThread.OpenQuery;
//begin
//  try
//    FQuery.Open;
//  except
//    on E : Exception do
//      LastErrorMessage := E.Message;
//  end;
//end;

//procedure TBaseADOQueryThread.CloseQuery;
//begin
//  if FQuery.Active then
//    FQuery.Close;
//end;

{ TSQLCountThread }

//constructor TSQLCountThread.Create(const U, P, D, ATableName: string; CountEvent : TSQLCountEvent);
//begin
//  inherited Create(U, P, D);
//  FSQLCountEvent := CountEvent;
//  FTableName := ATableName;
//  FreeOnTerminate := True;
//  Resume;
//end;

//procedure TSQLCountThread.DoCount;
//begin
//  if Assigned(FSQLCountEvent) then
//    FSQLCountEvent(FTableName, FCount);
//end;

//procedure TSQLCountThread.Execute;
//begin
//  inherited;
//  ExecuteCount;
//end;

//procedure TSQLCountThread.ExecuteCount;
//begin
//  if not Terminated then begin
//    with FQuery do try
//      SQL.Clear;
//      SQL.Add(Format('SELECT COUNT(*) FROM %s',[FTableName]));
//      try
//        Open;
//        FCount := Fields[0].AsInteger;
//        Synchronize(DoCount);
//        Close;
//      except
//        on E : Exception do
//          LastErrorMessage := E.Message;
//      end;
//    finally
//      Close;
//    end;
//  end;
//end;

{ TSQLRowList }

procedure TSQLRowList.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

constructor TSQLRowList.Create(const ATableName: string;
  const FieldNames: array of string);
var
  i : integer;
begin
  inherited Create;
  FDisableSorting := False;
  FTableName := ATableName;
  FFields := TFieldInfoList.Create;
  FUserFilterField := '';
  FUserFilterValue := NULL;
  for i := Low(FieldNames) to High(FieldNames) do
    FFields.Add(TFieldInfo.Create(FieldNames[i]));
end;

constructor TSQLRowList.Create(const ATableName: string;
  const Fields: array of TFieldQueryInfo);
var
  i : integer;
begin
  inherited Create;
  FDisableSorting := False;
  FTableName := ATableName;
  FFields := TFieldInfoList.Create;
  for i := Low(Fields) to High(Fields) do
    FFields.Add(TFieldInfo.Create(
      Fields[i].FieldName,
      Fields[i].Visible,
      Fields[i].Filter,
      Fields[i].FilterValue,
      Fields[i].FilterPredicate));
end;

constructor TSQLRowList.Create(const ATableName: string);
begin
  inherited Create;
  FDisableSorting := False;
  FTableName := ATableName;
  FFields := TFieldInfoList.Create;
end;

procedure TSQLRowList.SetFields(const Fields: array of TFieldQueryInfo);
var
  i : integer;
begin
  Clear;
  for i := Low(Fields) to High(Fields) do
    FFields.Add(TFieldInfo.Create(
      Fields[i].FieldName,
      Fields[i].Visible,
      Fields[i].Filter,
      Fields[i].FilterValue,
      Fields[i].FilterPredicate));
end;

procedure TSQLRowList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TRowData(Items[i]).Free;
  inherited;
end;

destructor TSQLRowList.Destroy;
begin
  FFields.Free;
  inherited;
end;

procedure TSQLRowList.SetReading(const Value: boolean);
begin
  FReading := Value;
end;

procedure TSQLRowList.SetRecordCount(const Value: integer);
begin
  FRecordCount := Value;
end;

function TSQLRowList.GetRow(index: integer): TRowData;
begin
  Result := TRowData(Items[index]);
end;

procedure TSQLRowList.SetRow(index: integer; const Value: TRowData);
begin
  Items[index] := Value;
end;

function TSQLRowList.Clone: TSQLRowList;
var
  i : integer;
  FA : array of string;
begin
  SetLength(FA, FFields.Count);
  for i := 0 to FFields.Count - 1 do
    FA[i] := FFields[i].Name;
  Result := TSQLRowList.Create(TableName, FA);
end;

procedure TSQLRowList.AddRows(RowList: TSQLRowList);
var
  i, idx : integer;
begin
  for i := 0 to RowList.Count - 1 do begin
    idx := Add(TRowData.Create(RowList[i].FieldCount));
    TRowData(Items[idx]).Assign(RowList[i]);
  end;
  { Fire this only when we flush the record buffer - way less flicker }
//  if RowList.Count > 0 then
    if Assigned(FOnAdd) then
      FOnAdd(Self);
end;

procedure TSQLRowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
//  if Action = lnAdded then
//    if Assigned(FOnAdd) then
//      FOnAdd(Self);
  inherited Notify(Ptr, Action);
end;

procedure TSQLRowList.SetOnAdd(const Value: TNotifyEvent);
begin
  FOnAdd := Value;
end;

procedure TSQLRowList.SetUserFilterValue(const Value: Variant);
begin
  FUserFilterValue := Value;
end;

procedure TSQLRowList.SetUserFilterField(const Value: string);
begin
  FUserFilterField := Value;
end;

function TSQLRowList.GetVisibleCount: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Fields.Count - 1 do
    if Fields[i].Visible then
      Inc(Result);
end;

procedure TSQLRowList.SaveToFile(const FileName: string);
type
  TPadSide = (psLeft, psRight);
const
  CRLF = #13+#10;
var
  FS : TFileStream;
  i, j, k, TempLen : integer;
  s, s1, s2, DataStr : string;

  function SpacePad(const s : string; StrLen : integer; PadSide : TPadSide):string;
  begin
    Result := s;
    while Length(Result) < StrLen do
      case PadSide of
        psLeft : Result := ' '+Result;
        psRight : Result := Result+' ';
      end;
    if Length(Result) > StrLen then
      Result := Copy(Result,1,StrLen);
  end;

  function FillString(const AChar : Char; StrLen : integer):string;
  begin
    Result := '';
    while Length(Result) < StrLen do
      Result := Result + AChar;
  end;

begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  try
    s1 := '';
    s2 := '';
    for i := 0 to Fields.Count - 1 do begin
      TempLen := iif_int(Length(Fields[i].Name)>Fields[i].Length,
                            Length(Fields[i].Name),
                            Fields[i].Length);
      if Fields[i].FieldType = ftString then begin
        s1 := s1 + ' ' + SpacePad(Fields[i].Name, TempLen, psLeft);
        s2 := s2 + ' ' + FillString('=',TempLen);
      end
      else begin
        s1 := s1 + ' ' + SpacePad(Fields[i].Name, TempLen, psLeft);
        s2 := s2 + ' ' + FillString('=',TempLen);
      end;
    end;
    s1 := s1 + CRLF;
    s2 := s2 + CRLF;
    FS.Write(s1[1],Length(s1));
    FS.Write(s2[1],Length(s2));
    { Do data rows }
    for j := 0 to Count - 1 do begin
      DataStr := '';
      for k := 0 to Fields.Count - 1 do begin
        TempLen := iif_int(Length(Fields[k].Name)>Fields[k].Length,
                              Length(Fields[k].Name),
                              Fields[k].Length);
        DataStr := DataStr + ' ' + SpacePad(VarToStr(Row[j].Data[k]), TempLen, psLeft);
      end;
      DataStr := DataStr + CRLF;
      FS.Write(DataStr[1],Length(DataStr));
    end;
  finally
    FS.Free;
  end;
end;

procedure TSQLRowList.SetDisableSorting(const Value: boolean);
begin
  FDisableSorting := Value;
end;

{ TSQLQueryThread }

//constructor TSQLQueryThread.Create(const U, P, D: string;
//  ARowList : TSQLRowList; const BufferSize : integer);
//begin
//  if ARowList = nil then
//    raise Exception.Create('Rowlist not initialized');
//  inherited Create(U, P, D, ARowList.TableName, nil);
//  FSortStack := TSortStack.Create(MAX_SORT_COL);
//  FFieldList := TFieldInfoList.Create;
//  FRowList := ARowList;
//  FRowBuffer := ARowList.Clone;
//  FBufferSize := BufferSize;
//  FSearchChange := False;
//  FSearchCaseSensitive := False;
//  FHaveMetaData := False;
//  FNeedParams := False;
//  FCheckChangeThreshold := DefCheckChangeThreshold;
//end;

//destructor TSQLQueryThread.Destroy;
//begin
//  SyncDebugMessage('TSQLQueryThread.Destroy - Start');
//
//  if Assigned(FRowBuffer) then
//    FRowBuffer.Free;
//  FRowList := nil;
//  FFieldList.Free;
//
//  FSortStack.Free;
//
//  SyncDebugMessage('TSQLQueryThread.Destroy - End (Except for inherited)');
//  inherited;
//end;

//procedure TSQLQueryThread.UpdateSearchCriteria(const SearchField: string;
//  const SearchValue: Variant; const SearchPredicate: TWherePredicate;
//  CaseSensitive : boolean = False);
//begin
//  EnterCriticalSection(UpdateCriticalSection);
//  try
//    FSearchField := SearchField;
//    FSearchValue := SearchValue;
//    FSearchPredicidate := SearchPredicate;
//    FSearchCaseSensitive := CaseSensitive;
//    FSearchChange := True;
//  finally
//    LeaveCriticalSection(UpdateCriticalSection);
//  end;
//end;

//procedure TSQLQueryThread.AddSearchField(const FieldName: string);
//begin
//  if Length(FieldName) > 0 then begin
//    EnterCriticalSection(SortCriticalSection);
//    try
//      FSortStack.Pop(FieldName);
//      FSearchChange := True;
//    finally
//      LeaveCriticalSection(SortCriticalSection);
//    end;
//  end;
//end;

//procedure TSQLQueryThread.DoCount;
//begin
//  inherited;
//  FRowList.RecordCount := FCount;
//end;

//function TSQLQueryThread.FieldsString: string;
//var
//  i : integer;
//begin
//  Result := '';
//  for i := 0 to FRowList.Fields.Count - 1 do begin
//    if i = 0 then
//      Result := Result + FRowList.Fields[i].Name
//    else
//      Result := Result + ',' + FRowList.Fields[i].Name;
//  end;
//end;

//function TSQLQueryThread.WhereClause: string;
//var
//  i, WhereCount : integer;
//begin
//  WhereCount := 0;
//  Result := '';
//  { First add any design time conditions }
//  for i := 0 to FRowList.Fields.Count - 1 do
//    if FRowList.Fields[i].Filtered then begin
//      Inc(WhereCount);
//      Result := iif_str(WhereCount > 1, 'AND ','WHERE ');
////      if WhereCount > 1 then
////        Result := 'WHERE '
////      else
////        Result := 'AND ';
//      Result := Format('%s %s %s :%s', [
//                  Result,
//                  FRowList.Fields[i].Name,
//                  WherePredicates[FRowList.Fields[i].WherePredicate],
//                  FRowList.Fields[i].Name
//                  ]);
//      if not FNeedParams then
//        FNeedParams := True;
//    end;
//  { Second add filter if it exists }
//  EnterCriticalSection(UpdateCriticalSection);
//  try
//    if FSearchChange then begin
//      //if not (VarIsNull(FSearchValue) or VarIsEmpty(FSearchValue)) then begin
//      if Length(VarToStr(FSearchValue)) > 0 then begin
//        { Add condition }
//        if Length(Result) > 0 then
//          Result := Format('%s AND %s %s :%s',[
//                      Result,
//                      {TODO: Handle CLOBs more efficiently  }
////                      iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(%s)',[FSearchField])),
//                      iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),
//                      WherePredicates[FSearchPredicidate],
//                      FSearchField])
//        else
//          Result := Format('WHERE %s %s :%s',[
//                      {TODO: Handle CLOBs more efficiently  }
////                      iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(%s)',[FSearchField])),
//                      iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),
//                      WherePredicates[FSearchPredicidate],
//                      FSearchField]);
//        { Notify we need parameters }
//        if not FNeedParams then
//          FNeedParams := True;
//      end;
//      { Reset Changed flag }
//      FSearchChange := False;
//    end;
//  finally
//    LeaveCriticalSection(UpdateCriticalSection);
//  end;
//end;

//function TSQLQueryThread.CheckForSearchCriteria:boolean;
//begin
//  EnterCriticalSection(UpdateCriticalSection);
//  try
//    Result := FSearchChange;
//  finally
//    LeaveCriticalSection(UpdateCriticalSection);
//  end;
//end;

//function TSQLQueryThread.QueryFetch:TFetchOutcome;
//var
//  i, idx, rowidx, cnt : integer;
//begin
//  SyncMessage('Reading records...');
//  try
//    if not FQuery.Active then Exit;
//    while ((not FQuery.Eof) and (not Terminated)) do begin
//      if CheckForSearchCriteria then begin
//        Result := foChangeInterupt;
//        Break;
//      end;
//      Inc(cnt);
//      idx := FRowBuffer.Add(TRowData.Create(FRowList.Fields.Count));
////      idx := FRowBuffer.Add(TRowData.Create(FRowList.VisibleCount));
//      rowidx := 0;
//      for i := 0 to FRowList.Fields.Count - 1 do begin
////      for i := 0 to FRowList.VisibleCount - 1 do begin
////        if FRowList.Fields[i].Visible then begin
//          if FRowList.Fields[i].FieldType = ftMemo then
//            FRowBuffer[idx].Data[i] := FQuery.FieldByName(FRowList.Fields[i].Name).AsString
//          else
//            FRowBuffer[idx].Data[i] := FQuery.FieldByName(FRowList.Fields[i].Name).Value;
////          Inc(rowidx);
////        end;
////          case FRowList.Fields[i].FieldType of
////            { Integer Types }
////            ftInteger, ftLargeint, ftSmallint :
////              FRowBuffer[idx].Data[i] :=
////                FQuery.FieldByName(FRowList.Fields[i].Name).AsInteger;
////            { Float Types }
////            ftFloat :
////              FRowBuffer[idx].Data[i] :=
////                FQuery.FieldByName(FRowList.Fields[i].Name).AsFloat;
////            { Date Types }
////            ftDate, ftDateTime :
////              FRowBuffer[idx].Data[i] :=
////                FQuery.FieldByName(FRowList.Fields[i].Name).AsDateTime;
////            { String Types }
////            ftString, ftMemo :
////              FRowBuffer[idx].Data[i] :=
////                FQuery.FieldByName(FRowList.Fields[i].Name).AsString;
////          else
////            FRowBuffer[idx].Data[i] :=
////              FQuery.FieldByName(FRowList.Fields[i].Name).AsString;
////          end;
//      end;
//      if cnt mod FBufferSize = 0 then
//        Synchronize(FlushBuffer);
//      FQuery.Next;
//    end;
//    if FQuery.Eof then
//      Result := foEof;
//    Synchronize(FlushBuffer);
//  except
//    on E : Exception do begin
//      LastErrorMessage := E.Message;
//      Result := foEof;
//    end;
//  end;
//  SyncMessage('');
//end;

//procedure TSQLQueryThread.SetQueryParameters;
//var
//  i : integer;
//begin
//  SyncDebugMessage('TSQLQueryThread.SetQueryParameters - Start');
//  if FNeedParams then begin
//    with FQuery do begin
//      EnterCriticalSection(UpdateCriticalSection);
//      try
//        try
//          Prepared := True;
//          { User input filter }
//          if Length(VarToStr(FSearchValue)) > 0 then
//            Parameters.ParamByName(FSearchField).Value := FSearchValue;
//
//          { Design time filters }
//          for i := 0 to FRowList.Fields.Count - 1 do begin
//            if FRowList.Fields[i].Filtered then begin
//              Parameters.ParamByName(FRowList.Fields[i].Name).Value := FRowList.Fields[i].FilterValue;
//            end;
//          end;
//          {
//          for i := 0 to Parameters.Count - 1 do begin
//            // Deal with this later
//          end;
//          }
//          FNeedParams := False;
//        except
//          on E : Exception do
//            LastErrorMessage := E.Message;
//        end;
//      finally
//        LeaveCriticalSection(UpdateCriticalSection);
//      end;
//    end;
//  end;
//  SyncDebugMessage('TSQLQueryThread.SetQueryParameters - End');
//end;

//procedure TSQLQueryThread.ClearRowList;
//begin
//  if FRowList <> nil then
//    FRowList.Clear;
//  if FRowBuffer <> nil then
//    FRowBuffer.Clear;
//end;

//function TSQLQueryThread.DoFetchQuery:boolean;
//var
//  s : string;
//begin
//  SyncMessage('Querying Database...');
//  with FQuery do begin
//    try
//      if Active then Close;
//      SQL.Clear;
//      { Start reading }
//      if not FRowList.DisableSorting then
//        s := Format('SELECT %s FROM %s %s ORDER BY %s WITH UR',[
//          FieldsString,
//          FRowList.TableName,
//          WhereClause,
//          FSortStack.SortFields])
//      else
//        s := Format('SELECT %s FROM %s %s WITH UR',[
//          FieldsString,
//          FRowList.TableName,
//          WhereClause]);
//      SyncDebugMessage(s);
//      SQL.Add(s);
//      try
//        try
//          ClearRowList;
//          SetQueryParameters;
//          Open;
//          case QueryFetch of
//            foEof : Result := True;
//            foChangeInterupt : Result := False;
//            { Any exception message from the fetch would have already been displayed }
//            foException : Result := True;
//          end;
//        except
//          on E : Exception do begin
//            LastErrorMessage := E.Message;
//            raise;
//          end;
//        end;
//      finally
//        Close;
//      end;
//    except
//      on E : Exception do begin
//        Result := True;
//        LastErrorMessage := E.Message;
//      end;
//    end;
//  end;
//  SyncMessage('');
//end;

//procedure TSQLQueryThread.Execute;
//var
////  i, j, idx, cnt : integer;
////  s, s1 : string;
////  RowData : TRowData;
//  Done, FetchComplete : boolean;
//begin
//  inherited;
//  SyncDebugMessage('TSQLQueryThread.Execute - Start');
//  FetchComplete := False;
////  cnt := 0;
//  if not Terminated then
//    if ReadMetaData then
//      repeat
//        if not FetchComplete or CheckForSearchCriteria then begin
//          repeat
//            FetchComplete := DoFetchQuery;
//          until FetchComplete or Terminated;
//        end;
//        Sleep(250); // One quarter of a second
//      until Terminated;
//  SyncDebugMessage('TSQLQueryThread.Execute - End;');
//
////      SyncDebugMessage('Generate Query');
////      with FQuery do
////        try
////          //SyncDebugMessage(Format('Generate query for %s',[FRowList.TableName]));
//////          SyncMessage('Counting records...');
//////          SQL.Add(Format('SELECT COUNT(*) FROM %s WITH UR',[FRowList.TableName]));
//////          try
//////            Open;
//////            if not IsEmpty then
//////              FRowList.RecordCount := Fields[0].AsInteger;
//////            Close;
//////            SyncDebugMessage(Format('Count %d',[FRowList.RecordCount]));
//////          except
//////            on E : Exception do
//////              LastErrorMessage := E.Message;
//////          end;
////          { Reset the query }
////          if Active then Close;
////          SQL.Clear;
////          { Start reading }
////          s1 := Format('SELECT %s FROM %s %s WITH UR',[
////            FieldsString,
////            FRowList.TableName,
////            WhereClause]);
////          SyncDebugMessage(s1);
////          SQL.Add(s1);
////          try
////            //SQL.Add(Format('SELECT %s FROM %s WITH UR',[s, FRowList.TableName]));
////            SyncDebugMessage('Start Reading');
////            SyncMessage('Reading records...');
////            try
////              Open;
////              while ((not Eof) and (not Terminated)) do begin
////                Inc(cnt);
////                idx := FRowBuffer.Add(TRowData.Create(FRowList.Fields.Count));
////                for j := 0 to FRowList.Fields.Count - 1 do begin
////                  case FRowList.Fields[j].FieldType of
////                    { Integer Types }
////                    ftInteger, ftLargeint, ftSmallint :
////                      begin
////                        FRowBuffer[idx].Data[j] := FieldByName(FRowList.Fields[j].Name).AsInteger;
////                      end;
////                    { Float Types }
////                    ftFloat :
////                      begin
////                        FRowBuffer[idx].Data[j] := FieldByName(FRowList.Fields[j].Name).AsFloat;
////                      end;
////                    { Date Types }
////                    ftDate, ftDateTime :
////                      begin
////                        FRowBuffer[idx].Data[j] := FieldByName(FRowList.Fields[j].Name).AsDateTime;
////                      end;
////                    { String Types }
////                    ftString, ftMemo :
////                      begin
////                        FRowBuffer[idx].Data[j] := FieldByName(FRowList.Fields[j].Name).AsString;
////                      end;
////                  else
////                    FRowBuffer[idx].Data[j] := FieldByName(FRowList.Fields[j].Name).AsString;
////                  end;
////                end;
////                if cnt mod FBufferSize = 0 then
////                  Synchronize(FlushBuffer);
////                //Synchronize({Write to list buffer});
////                Next;
////              end;
////              Synchronize(FlushBuffer);
////            finally
////              Close;
////              SyncMessage('Done');
////            end;
////          except
////            on E : Exception do
////              LastErrorMessage := E.Message;
////          end;
////        finally
////          Close;
////        end; // with FQuery do try...
////    end;     // if ReadMetaData then...
////  SyncDebugMessage('Finish Execute');
//end;

//procedure TSQLQueryThread.FlushBuffer;
//var
//  i : integer;
//begin
////  if FRowBuffer.Count = 0 then begin
////    { Assume Eof here }
////    FRowList.Clear;
////    FRowBuffer.Clear;
////  end
////  else begin
//    { Write the records from the buffer to rowlist }
//    FRowList.AddRows(FRowBuffer);
//    { Clear the buffer }
//    FRowBuffer.Clear;
////  end;
//end;

//function TSQLQueryThread.ReadMetaData:boolean;
//var
//  i : integer;
//begin
////  SyncDebugMessage('TSQLQueryThread.ReadMetaData - Start');
//  SyncMessage('Reading meta data...');
//  Result := False;
//  if FHaveMetaData then begin
//    Result := True;
//    Exit;
//  end
//  else
//  begin
////    with TADOQuery.Create(nil) do try
//    with FQuery do begin
////      Connection := FDB;
//      for i := 0 to FRowList.Fields.Count - 1 do begin
//        if Active then Close;
//        SQL.Clear;                { schema name } { table name }      { column name }
//        SQL.Add(Format(SQLColDef,[FDB.SchemaName, FRowList.TableName, FRowList.Fields[i].Name]));
//  //      SyncDebugMessage(Format('Get Field Data for %s',[FRowList.Fields[i].Name]));
//        try
//          Open;
//          if not IsEmpty then begin
//            FRowList.Fields[i].FieldTypeName := FieldByName('TYPENAME').AsString;
//            FRowList.Fields[i].Scale := FieldByName('SCALE').AsInteger;
//            FRowList.Fields[i].Length := FieldByName('LENGTH').AsInteger;
//          end
//          else
//            raise Exception.CreateFmt('QueryThread - Field %s not found',[FRowList.Fields[i].Name]);
//          Close;
//          Result := True;
//        except
//          on E : Exception do
//            LastErrorMessage := E.Message;
//        end;
//      end;
////    finally
////      Free;
////    end;
//    end;
//    FHaveMetaData := Result;
//  end;
//  SyncMessage('');
////  SyncDebugMessage('TSQLQueryThread.ReadMetaData - End');
//end;

{ TFieldInfo }

procedure TFieldInfo.SetLength(const Value: integer);
begin
  FLength := Value;
end;

procedure TFieldInfo.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TFieldInfo.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TFieldInfo.SetFieldTypeName(const Value: string);
begin
  FFieldTypeName := Value;
  if FFieldTypeName = 'BIGINT' then FieldType := ftLargeint
  else if FFieldTypeName = 'BLOB' then FieldType := ftBlob
  else if FFieldTypeName = 'CHARACTER' then FieldType := ftString
  else if FFieldTypeName = 'CLOB' then FieldType := ftMemo
  else if FFieldTypeName = 'DATE' then FieldType := ftDate
  else if FFieldTypeName = 'DECIMAL' then FieldType := ftFloat
  else if FFieldTypeName = 'DOUBLE' then FieldType := ftFloat
  else if FFieldTypeName = 'INTEGER' then FieldType := ftInteger
  else if FFieldTypeName = 'SMALLINT' then FieldType := ftSmallint
  else if FFieldTypeName = 'TIMESTAMP' then FieldType := ftDateTime
  else if FFieldTypeName = 'VARCHAR' then FieldType := ftString
  else FieldType := ftUnknown;
end;

procedure TFieldInfo.SetScale(const Value: integer);
begin
  FScale := Value;
end;

constructor TFieldInfo.Create(const AName, ATypeName: string; const AScale,
  ALength: integer);
begin
  inherited Create;
  FKeySequence := -1;
  Name := AName;
  FieldTypeName := ATypeName;
  Scale := AScale;
  Length := ALength;
end;

constructor TFieldInfo.Create(const AName: string);
begin
  inherited Create;
  FKeySequence := -1;
  Name := AName;
end;

constructor TFieldInfo.Create(const AName: string; AVisible, AFilter: boolean;
  AFilterValue: Variant; Predicate: TWherePredicate);
begin
  inherited Create;
  FKeySequence := -1;
  Name := AName;
  Visible := AVisible;
  Filtered := AFilter;
  FilterValue := AFilterValue;
  WherePredicate := Predicate;
end;

procedure TFieldInfo.SetFilterValue(const Value: Variant);
begin
  FFilterValue := Value;
end;

procedure TFieldInfo.SetKeySequence(const Value: integer);
begin
  FKeySequence := Value;
end;

procedure TFieldInfo.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

procedure TFieldInfo.SetFiltered(const Value: boolean);
begin
  FFiltered := Value;
end;

procedure TFieldInfo.SetWherePredicate(const Value: TWherePredicate);
begin
  FWherePredicate := Value;
end;

{ TFieldInfoList }

function TFieldInfoList.GetFieldInfo(index: integer): TFieldInfo;
begin
  Result := TFieldInfo(Items[index]);
end;

procedure TFieldInfoList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TFieldInfo(Items[i]).Free;
  inherited;
end;

function TFieldInfoList.FindField(const FieldName: string): TFieldInfo;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Fields[i].Name = FieldName then begin
      Result := Fields[i];
      Break;
    end;
end;

{ TRowData }

procedure TRowData.Assign(Source: TPersistent);
var
  i : integer;
begin
  if Source is TRowData then begin
    if FieldCount <> TRowData(Source).FieldCount then
      raise Exception.Create('TRowData.Assign - FieldCount must Match');
    for i := 0 to FieldCount - 1 do
      Data[i] := TRowData(Source).Data[i];
  end
  else
    inherited;
end;

function TRowData.AsString(index : integer) : string;
var
//  V : Variant;
  s : string;
begin
  Result := '';
//  WCCTrace(Format('idx: %d FFieldCount: ',[index, FFieldCount]));
  if index > FFieldCount - 1 then Exit;
//  V := Data[index];
//  s := VarToStr(V);
//  Result := s;
  Result := VarToStr(Data[index]);
end;

constructor TRowData.Create(const AFieldCount: integer);
begin
  inherited Create;
  FieldCount := AFieldCount;
end;

function TRowData.GetData(index: integer): Variant;
begin
  Result := FData[index];
end;

procedure TRowData.SetData(index: integer; const Value: Variant);
begin
  FData[index] := Value;
end;

procedure TRowData.SetFieldCount(const Value: integer);
begin
  FFieldCount := Value;
  SetLength(FData, FFieldCount);
end;

end.
