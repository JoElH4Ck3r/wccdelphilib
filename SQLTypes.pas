unit SQLTypes;

interface

uses Classes, Windows, Sysutils, DB, Variants, ADOINT, ADODB,
  SQLProf, CustomWCCConnection;

  // DBConnection, WCCDBServerTypes, RREDConnection;

type
                                            // Ordinals
  TWherePredicate  = (wpEquals,             // 0
                      wpNotEqualTo,         // 1
                      wpGreaterThan,        // 2
                      wpLessThan,           // 3
                      wpGreaterThanEqualTo, // 4
                      wpLessThanEqualTo,    // 5
                      wpLike,               // 6
                      wpIs                  // 7
                      );

const
  C_FALSE = 'False';
  C_TRUE  = 'True';

  WherePredicates : array[TWherePredicate] of string[4] = ('=','<>','>','<','>=','<=','LIKE','IS');

  SQLColDef = 'SELECT COLNAME, TYPENAME, LENGTH, SCALE, KEYSEQ '+
              'FROM SYSCAT.COLUMNS '+
              'WHERE TABSCHEMA = ''%s'' '+
              'AND TABNAME = ''%s'' '+
              'AND COLNAME = ''%s'' '+
              'WITH UR';

type
  TSortDirection = (sdAscending, sdDescending);

  TSortStack = class
  private
    FStack : array of string;
    FSorts : array of TSortDirection;
    function GetCount: integer;
    procedure Shift;
//    procedure Clear;
//    function GetFieldName(index: integer): string;
    function GetSortFields: string;
  public
    constructor Create(Size : integer);
    property Count : integer read GetCount;
    procedure Pop(AFieldName : string);
    property SortFields : string read GetSortFields;
  end;

  TFieldValue = class
  private
    FName: string;
    FValue: Variant;
    FFieldType: TFieldType;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure SetFieldType(const Value: TFieldType);
  public
    constructor Create(const AName : string; const AValue : Variant); virtual;
    property Name : string read FName write SetName;
    property Value : Variant read FValue write SetValue;
    property FieldType : TFieldType read FFieldType write SetFieldType;
  end;

  TFieldValueList = class(TList)
  private
    FAllowNulls: boolean;
    FIgnoreNulls: boolean;
    function GetFieldValue(index: integer): TFieldValue;
    procedure SetAllowNulls(const Value: boolean);
    procedure SetIgnoreNulls(const Value: boolean);
  public
    constructor Create;
    procedure Clear; override;
    procedure ClearValues;
    procedure UpdateParameters(P : TParameters);
    property FieldValues [index : integer] : TFieldValue read GetFieldValue; default;
    property AllowNulls : boolean read FAllowNulls write SetAllowNulls;
    property IgnoreNulls : boolean read FIgnoreNulls write SetIgnoreNulls;
  end;

  TWhereCondition = class(TPersistent)
  private
    FPredicate: TWherePredicate;
    FName: string;
    FValue: Variant;
    procedure SetPredicate(const Value: TWherePredicate);
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
  public
    constructor Create(const AName : string; const AValue : Variant; APredicate : TWherePredicate); overload;
    property Name : string read FName write SetName;
    property Value : Variant read FValue write SetValue;
    property Predicate : TWherePredicate read FPredicate write SetPredicate;
  end;

  TWhereConditionList = class(TList)
  private
    function GetWhereCondition(index: integer): TWhereCondition;
  public
    procedure Clear; override;
    property WhereCondition [index : integer] : TWhereCondition read GetWhereCondition; default;
  end;

  TUpdateQuery = class(TADOQuery)
  private
    FDB : TCustomWCCConnection;
    FUpdateFields : TFieldValueList;
    FKeyFields : TFieldValueList;
    FTableName : string;
    FHaveMetaData : boolean;
    FProfiler : TSQLProfiler;
  protected
    procedure GetMetaData; virtual;
    function GetSQL: string; virtual;
  public
    constructor Create(AOwner : TComponent; const ATableName : string;
      ADB : TCustomWCCConnection; AProfiler : TSQLProfiler); reintroduce;
    destructor Destroy; override;
    property TableName : string read FTableName;
    property UpdateFields : TFieldValueList read FUpdateFields;
    property KeyFields : TFieldValueList read FKeyFields;
    function ExecuteQuery:integer;
    function AddKeyField(const AField : string; const AValue : Variant):integer; overload;
    function AddKeyField(const AField : string):integer; overload;
    function AddUpdateField(const AField : string; const AValue : Variant):integer;
    function AddUpdate(const AField : string):integer; overload;
    procedure ClearValues;
    procedure ShowQuery;
  end;

  TInsertQuery = class(TADOQuery)
  private
    FTableName: string;
    FInsertFields: TFieldValueList;
    FDB : TCustomWCCConnection;
    FHaveMetaData : boolean;
    FProfiler : TSQLProfiler;
  protected
    procedure GetMetaData; virtual;
    function GetSQL: string; virtual;
  public
    constructor Create(AOwner : TComponent; const ATableName : string;
      ADB : TCustomWCCConnection; AProfiler : TSQLProfiler); reintroduce;
    destructor Destroy; override;
    property TableName : string read FTableName;
    function AddInsertField(const AField : string):integer; overload;
    function AddInsertField(const AField : string; const AValue : Variant):integer; overload;
    function UpdateParameter(const AField : string; const AValue : Variant):boolean;
    function ExecuteQuery:integer;
    procedure ClearValues;
    procedure PrepareQuery;
    procedure ShowQuery;
  end;

  TSQLUpdate = class(TPersistent)
  private
    FTableName: string;
    FKeyFields: TFieldValueList;
    FUpdateFields: TFieldValueList;
    function GetSQL: string;
    procedure SetTableName(const Value: string);
    procedure SetCLOBParam(P: TParameter; const s: string);
  protected
    procedure GetMetaData; virtual;
  public
    constructor Create(const ATableName : string);  overload;
    destructor Destroy; override;
    property TableName : string read FTableName write SetTableName;
    property UpdateFields : TFieldValueList read FUpdateFields write FUpdateFields;
    property KeyFields : TFieldValueList read FKeyFields write FKeyFields;
    //property SQL : string read GetSQL;
    function ExecuteQuery(Query : TADOQuery):integer;
    function AddKeyField(const AField : string; const AValue : Variant):integer;
    function AddUpdateField(const AField : string; const AValue : Variant):integer;
    procedure ShowQuery;
  end;

const
  DateParamFmtString : array[TDatabaseServer] of string = (
    'yyyy-mm-dd',
    'yyyymmdd'
    );

  DateTimeParamFmtString : array[TDatabaseServer] of string = (
    'yyyy-mm-dd-hh.nn.ss.zzz000',
    'yyyymmdd hh:nn:ss'
    );

function VarFieldToString(FieldType : TFieldType; Value : Variant):string;
function DB2FieldNameToFieldType(const DB2FieldTypeName: string):TFieldType;
procedure SetCLOBParam(P : TParameter; const s : string);
procedure SetBLOBParam(P : TParameter; const s : string);

procedure SetDateTimeParam(P : TParameter; DT : TDateTime); overload;
procedure SetDateTimeParam(P : TParameter; DT : TDateTime; ADS : TDatabaseServer); overload;

procedure SetDateParam(P : TParameter; DT : TDateTime); overload;
procedure SetDateParam(P : TParameter; DT : TDateTime; ADS : TDatabaseServer); overload;

implementation

uses WCCUtil, JclStrings;

function TrimCRLF(s : string):string;
var
  I, L: Integer;
begin
  Result := '';
  L := Length(s);
  I := 1;
  while ((s[I] = #13) or (s[I] = #10)) do
    Inc(I);
  while ((s[L] = #13) or (s[L] = #10)) do
    Dec(L);
  if I < L then
    Result := Copy(s,I,L);
end;

procedure SetCLOBParam(P : TParameter; const s : string);
var
  SS : TStringStream;
  Temp : string;
begin
  { Trim trailing spaces and CRLF's }
//  Temp := TrimCRLF(Trim(s));
  Temp := Trim(s);
  SS := TStringStream.Create(Temp);
  try
    P.LoadFromStream(SS, ftMemo);
  finally
    SS.Free;
  end;
end;

procedure SetBLOBParam(P : TParameter; const s : string);
var
  SS : TStringStream;
  Temp : string;
begin
  { Trim trailing spaces and CRLF's }
//  Temp := TrimCRLF(Trim(s));
  Temp := Trim(s);
  SS := TStringStream.Create(Temp);
  try
    P.LoadFromStream(SS, ftBlob);
  finally
    SS.Free;
  end;
end;

procedure SetDateTimeParam(P : TParameter; DT : TDateTime);
var
  PO : _Parameter;
begin
  PO            := P.ParameterObject;
  PO.Type_      := adVarChar;
  PO.Size       := 26;
  PO.Attributes := 0;
  P.DataType    := ftString;
  P.Size        := 26;
  P.Value       := FormatDateTime('yyyy-mm-dd-hh.nn.ss.zzz000',DT);
end;

procedure SetDateParam(P : TParameter; DT : TDateTime);
var
  PO : _Parameter;
begin
  PO            := P.ParameterObject;
  PO.Type_      := adVarChar;
  PO.Size       := 26;
  PO.Attributes := 0;
  P.DataType    := ftString;
  P.Size        := 26;
  P.Value       := FormatDateTime('yyyy-mm-dd',DT);
end;

procedure SetDateTimeParam(P : TParameter; DT : TDateTime; ADS : TDatabaseServer); overload;
var
  PO : _Parameter;
begin
  PO            := P.ParameterObject;
  PO.Type_      := adVarChar;
  PO.Size       := 26;
  PO.Attributes := 0;
  P.DataType    := ftString;
  P.Size        := 26;
  P.Value       := FormatDateTime(DateTimeParamFmtString[ADS],DT);
end;

procedure SetDateParam(P : TParameter; DT : TDateTime; ADS : TDatabaseServer); overload;
var
  PO : _Parameter;
begin
  PO            := P.ParameterObject;
  PO.Type_      := adVarChar;
  PO.Size       := 26;
  PO.Attributes := 0;
  P.DataType    := ftString;
  P.Size        := 26;
  P.Value       := FormatDateTime(DateParamFmtString[ADS],DT);
end;


function DB2FieldNameToFieldType(const DB2FieldTypeName: string):TFieldType;
begin
  if DB2FieldTypeName = 'BIGINT' then Result := ftLargeint
  else if DB2FieldTypeName = 'BLOB' then Result := ftBlob
  else if DB2FieldTypeName = 'CHARACTER' then Result := ftString
  else if DB2FieldTypeName = 'CLOB' then Result := ftMemo
  else if DB2FieldTypeName = 'DATE' then Result := ftDate
  else if DB2FieldTypeName = 'DECIMAL' then Result := ftFloat
  else if DB2FieldTypeName = 'DOUBLE' then Result := ftFloat
  else if DB2FieldTypeName = 'INTEGER' then Result := ftInteger
  else if DB2FieldTypeName = 'SMALLINT' then Result := ftSmallint
  else if DB2FieldTypeName = 'TIMESTAMP' then Result := ftDateTime
  else if DB2FieldTypeName = 'VARCHAR' then Result := ftString
  else Result := ftUnknown;
end;

function VarFieldToString(FieldType : TFieldType; Value : Variant):string;
begin
  { Good place to add custom handling if needed }
  Result := VarToStr(Value);
//  case FieldType of
////    ftUnknown : Result := Value;
//    ftString : Result := Value;
//    ftSmallint : Result := Value;
//    ftInteger : Result := Value;
//    ftWord : Result := Value;
////    ftBoolean : Result := Value;
//    ftFloat : Result := Value;
////    ftCurrency : Result := Value;
////    ftBCD : Result := Value;
//    ftDate : Result := Value;
//    ftTime : Result := Value;
//    ftDateTime : Result := Value;
////    ftBytes : Result := Value;
////    ftVarBytes : Result := Value;
////    ftAutoInc : Result := Value;
////    ftBlob : Result := Value;
//    ftMemo : Result := Value;
////    ftGraphic : Result := Value;
////    ftFmtMemo : Result := Value;
////    ftParadoxOle : Result := Value;
////    ftDBaseOle : Result := Value;
////    ftTypedBinary : Result := Value;
////    ftCursor : Result := Value;
////    ftFixedChar : Result := Value;
////    ftWideString : Result := Value;
//    ftLargeint : Result := Value;
////    ftADT : Result := Value;
////    ftArray : Result := Value;
////    ftReference : Result := Value;
////    ftDataSet : Result := Value;
////    ftOraBlob : Result := Value;
////    ftOraClob : Result := Value;
////    ftVariant : Result := Value;
////    ftInterface : Result := Value;
////    ftIDispatch : Result := Value;
////    ftGuid : Result := Value;
////    ftTimeStamp : Result := Value;
////    ftFMTBcd : Result := Value;
//  else
//    Result := Value;
//  end;
end;

{ TFieldValue }

procedure TFieldValue.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TFieldValue.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TFieldValue.Create(const AName: string; const AValue: Variant);
begin
  inherited Create;
  Name := AName;
  Value := AValue;
end;

procedure TFieldValue.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

{ TFieldValueList }

constructor TFieldValueList.Create;
begin
  inherited Create;
  FAllowNulls := True;
  FIgnoreNulls := True;
end;

function TFieldValueList.GetFieldValue(index: integer): TFieldValue;
begin
  Result := TFieldValue(Items[index]);
end;

procedure TFieldValueList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TFieldValue(Items[i]).Free;
  inherited;
end;

procedure TFieldValueList.UpdateParameters(P : TParameters);
var
  i, cnt : integer;
begin
  cnt := 0;
  for i := 0 to Count - 1 do begin
    if not (not IgnoreNulls and VarIsNull(FieldValues[i].Value)) then begin
      case FieldValues[i].FieldType of
        { Handle exceptions like CLOB and TIMESTAMP }
        ftMemo : SetCLOBParam(P.ParamByName(FieldValues[i].Name),
                              VarToStr(FieldValues[i].Value));
        ftDateTime : SetDateTimeParam(P.ParamByName(FieldValues[i].Name),
                                      VarToDateTime(FieldValues[i].Value));
        ftString : P.ParamByName(FieldValues[i].Name).Value := NullStr(FieldValues[i].Value);
      else
        { This will be the default }
        P.ParamByName(FieldValues[i].Name).Value := FieldValues[i].Value;
      end;
      Inc(cnt);
      WCCTrace(Format('%s = %s',[FieldValues[i].Name, VarToStr(FieldValues[i].Value)]));
    end;
  end;
  if ((cnt = 0) and (not AllowNulls)) then
    raise Exception.Create('NULL error: no fields updated');
end;

procedure TFieldValueList.ClearValues;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    FieldValues[i].Value := Null;
end;

procedure TFieldValueList.SetAllowNulls(const Value: boolean);
begin
  FAllowNulls := Value;
end;

procedure TFieldValueList.SetIgnoreNulls(const Value: boolean);
begin
  FIgnoreNulls := Value;
end;

{ TWhereCondition }

procedure TWhereCondition.SetPredicate(const Value: TWherePredicate);
begin
  FPredicate := Value;
end;

procedure TWhereCondition.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TWhereCondition.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TWhereCondition.Create(const AName: string; const AValue: Variant;
  APredicate: TWherePredicate);
begin
  inherited Create;
  Name := AName;
  Value := AValue;
  Predicate := APredicate;
end;

{ TWhereConditionList }

function TWhereConditionList.GetWhereCondition(index: integer): TWhereCondition;
begin
  Result := TWhereCondition(Items[index]);
end;

procedure TWhereConditionList.Clear;
var
  i : integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    TWhereCondition(Items[i]).Free;
end;

{ TInsertQuery }

constructor TInsertQuery.Create(AOwner: TComponent; const ATableName: string;
  ADB: TCustomWCCConnection; AProfiler: TSQLProfiler);
begin
  inherited Create(AOwner);
  FProfiler := AProfiler;
  FHaveMetaData := False;
  FDB := ADB;
  Connection := ADB;
  FTableName := ATableName;
  FInsertFields := TFieldValueList.Create;
  FInsertFields.IgnoreNulls := False;
end;

function TInsertQuery.GetSQL: string;
var
  i, j : integer;
  s1, s2 : string;
  Fields : TStringList;
begin
  if FInsertFields.Count = 0 then
    raise Exception.Create('No fields to insert');

  Fields := TStringList.Create;
  try
    for i := 0 to FInsertFields.Count - 1 do
      Fields.Add(FInsertFields[i].Name);
    s1 := StringsToStr(Fields,',',False);
    s2 := ':'+StringsToStr(Fields,',:',False);
  finally
    Fields.Free;
  end;

  Result := Format('INSERT INTO %s(%s) VALUES(%s)',[FTableName,s1,s2]);

  WCCTrace(Result);
end;

procedure TInsertQuery.GetMetaData;
var
   i, j : integer;
begin
  if FHaveMetaData then Exit;
  if Active then Close;
  for i := 0 to FInsertFields.Count - 1 do begin
    Close;
    SQL.Clear;
    SQL.Add(Format(SQLColDef,[FDB. SchemaName, FTableName, FInsertFields[i].Name]));
    try
      Open;
      if not Eof then
        FInsertFields[i].FieldType := DB2FieldNameToFieldType(FieldByName('TYPENAME').AsString);
      try
      except
        on E : Exception do begin
          WCCTrace('Error: TUpdateQuery.GetMetaData: '+E.Message);
          FInsertFields[i].FieldType := ftUnknown;
        end;
      end;
    finally
      Close;
    end;
  end;
end;

destructor TInsertQuery.Destroy;
begin
  FInsertFields.Free;
  inherited;
end;

procedure TInsertQuery.ShowQuery;
begin
  {  }
end;

procedure TInsertQuery.ClearValues;
begin
  FInsertFields.ClearValues;
end;

procedure TInsertQuery.PrepareQuery;
begin
  if not FHaveMetaData then
    GetMetaData;
  if Active then Close;
  SQL.Clear;
  SQL.Add(GetSQL);
  //SQL.SaveToFile('c:\Insert.sql');
  Self.Prepared := True;
end;

function TInsertQuery.ExecuteQuery: integer;
begin
//  if not FHaveMetaData then
//    GetMetaData;
//  if Active then Close;
//  SQL.Clear;
//  SQL.Add(GetSQL);
//  Self.Prepared := True;
  FInsertFields.UpdateParameters(Self.Parameters);
  Result := FProfiler.ProfileADOExec(Self);
end;

function TInsertQuery.AddInsertField(const AField: string;
  const AValue: Variant): integer;
begin
  Result := FInsertFields.Add(TFieldValue.Create(AField, Null));
end;

function TInsertQuery.AddInsertField(const AField: string): integer;
begin
  Result := FInsertFields.Add(TFieldValue.Create(AField, Null));
end;

function TInsertQuery.UpdateParameter(const AField: string;
  const AValue: Variant): boolean;
var
  i : integer;
begin
  if not (not FInsertFields.IgnoreNulls and VarIsNull(AValue)) then begin
    for i := 0 to FInsertFields.Count - 1 do begin
      if FInsertFields[i].Name = AField then begin
        case FInsertFields[i].FieldType of
          { Handle exceptions like CLOB and TIMESTAMP }
          ftMemo : SetCLOBParam(Parameters.ParamByName(AField), VarToStr(AValue));
          ftDateTime : SetDateTimeParam(Parameters.ParamByName(AField), VarToDateTime(AValue));
          ftString : Parameters.ParamByName(AField).Value := NullStr(AValue);
        else
          { This will be the default }
          Parameters.ParamByName(AField).Value := AValue;
        end;
        //WCCTrace(Format('%s = %s',[AField, VarToStr(AValue)]));

        Break;
      end;
    end;
  end;
end;

{ TUpdateQuery }

constructor TUpdateQuery.Create(AOwner: TComponent; const ATableName: string;
  ADB: TCustomWCCConnection; AProfiler : TSQLProfiler);
begin
  inherited Create(AOwner);
  FProfiler := AProfiler;
  FHaveMetaData := False;
  FDB := ADB;
  Connection := ADB;
  FTableName := ATableName;
  FKeyFields := TFieldValueList.Create;
  FUpdateFields := TFieldValueList.Create;
end;

destructor TUpdateQuery.Destroy;
begin
  FKeyFields.Free;
  FUpdateFields.Free;
  inherited;
end;

procedure TUpdateQuery.ShowQuery;
begin
  //ShowMessage(GetSQL);
end;

function TUpdateQuery.ExecuteQuery: integer;
//var
//  i, j : integer;
begin
  //Result := -1;
  { Read meta data for datatypes }
  if not FHaveMetaData then
    GetMetaData;
  if Active then Close;
  SQL.Clear;
  SQL.Add(GetSQL);
  Self.Prepared := True;
  FUpdateFields.UpdateParameters(Self.Parameters);
  FKeyFields.UpdateParameters(Self.Parameters);
//  for i := 0 to FUpdateFields.Count - 1 do
//    case
////    if FUpdateFields[i].Name = 'NOTES' then
////      SetCLOBParam(Parameters.ParamByName(FUpdateFields[i].Name), VarToStr(FUpdateFields[i].Value))
////    else   TParameters
//      Parameters.ParamByName(FUpdateFields[i].Name).Value := FUpdateFields[i].Value;
//  for j := 0 to FKeyFields.Count - 1 do
//    Parameters.ParamByName(FKeyFields[j].Name).Value := FKeyFields[j].Value;
  Result := FProfiler.ProfileADOExec(Self);//ExecSQL;
end;

function TUpdateQuery.AddKeyField(const AField: string;
  const AValue: Variant): integer;
begin
  Result := KeyFields.Add(TFieldValue.Create(AField, AValue));
end;

function TUpdateQuery.AddKeyField(const AField: string): integer;
begin
  Result := KeyFields.Add(TFieldValue.Create(AField, Null));
end;

function TUpdateQuery.AddUpdateField(const AField: string;
  const AValue: Variant): integer;
begin
  Result := UpdateFields.Add(TFieldValue.Create(AField, AValue));
end;

function TUpdateQuery.AddUpdate(const AField: string): integer;
begin
  Result := UpdateFields.Add(TFieldValue.Create(AField, Null));
end;

function TUpdateQuery.GetSQL: string;
var
  i, j : integer;
//  Field : string;
begin
  if FKeyFields.Count = 0 then
    raise Exception.Create('Key fields not specified');
  if FUpdateFields.Count = 0 then
    raise Exception.Create('No fields to update');

//  Result := Format('UPDATE %s SET '+#13,[FTableName]);
  Result := Format('UPDATE %s SET ',[FTableName]);

  for i := 0 to FUpdateFields.Count - 1 do
    if i = FUpdateFields.Count - 1 then
      //Result := Result + Format('%s = :%0:s '+#13,[FUpdateFields[i].Name])
      Result := Result + Format('%s = :%0:s ',[FUpdateFields[i].Name])
    else
      //Result := Result + Format('%s = :%0:s, '+#13,[FUpdateFields[i].Name]);
      Result := Result + Format('%s = :%0:s, ',[FUpdateFields[i].Name]);

  Result := Result + ' WHERE ';

  for j := 0 to FKeyFields.Count - 1 do
    if j = 0 then
      //Result := Result + Format('%s = :%0:s '+#13,[FKeyFields[j].Name])
      Result := Result + Format('%s = :%0:s ',[FKeyFields[j].Name])
    else
      //Result := Result + Format('AND %s = :%0:s '+#13,[FKeyFields[j].Name]);
      Result := Result + Format('AND %s = :%0:s ',[FKeyFields[j].Name]);

  WCCTrace(Result);
end;

procedure TUpdateQuery.GetMetaData;
var
  i, j : integer;
begin
  if FHaveMetaData then Exit;
  if Active then Close;
  for i := 0 to FUpdateFields.Count - 1 do begin
    Close;
    SQL.Clear;
    SQL.Add(Format(SQLColDef,[FDB.SchemaName, FTableName, FUpdateFields[i].Name]));
    try
      Open;
      if not Eof then
        FUpdateFields[i].FieldType := DB2FieldNameToFieldType(FieldByName('TYPENAME').AsString);
      try
      except
        on E : Exception do begin
          WCCTrace('Error: TUpdateQuery.GetMetaData: '+E.Message);
          FUpdateFields[i].FieldType := ftUnknown;
        end;
      end;
    finally
      Close;
    end;
  end;
  for j := 0 to FKeyFields.Count - 1 do begin
    Close;
    SQL.Clear;
    SQL.Add(Format(SQLColDef,[FDB.SchemaName, FTableName, FKeyFields[j].Name]));
    try
      Open;
      if not Eof then
        FKeyFields[j].FieldType := DB2FieldNameToFieldType(FieldByName('TYPENAME').AsString);
      try
      except
        on E : Exception do begin
          WCCTrace('Error: TUpdateQuery.GetMetaData: '+E.Message);
          FKeyFields[j].FieldType := ftUnknown;
        end;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TUpdateQuery.ClearValues;
begin
  FKeyFields.ClearValues;
  FUpdateFields.ClearValues;
end;

{ TSortStack }

constructor TSortStack.Create(Size : integer);
begin
  inherited Create;
  SetLength(FStack,Size);
  SetLength(FSorts,Size);
end;

function TSortStack.GetCount: integer;
begin
  Result := Length(FStack);
end;

//function TSortStack.GetFieldName(index: integer): string;
//begin
//  Result := FStack[index];
//end;

//procedure TSortStack.Clear;
//var
//  i : integer;
//begin
//  for i := Low(FStack) to High(FStack)-1 do
//    FStack[i] := '';
//end;

function TSortStack.GetSortFields: string;
var
  i : integer;
begin
  Result := '';
  for i := Low(FStack) to High(FStack) do
    if Length(FStack[i])> 0 then
      Result := Result + FStack[i] + iif_str(FSorts[i]=sdAscending,'',' DESC') + ',';
  Delete(Result,Length(Result),1);
end;

procedure TSortStack.Pop(AFieldName: string);
begin
  if AFieldName = FStack[0] then begin
    if FSorts[0] = sdDescending then
      FSorts[0] := sdAscending
    else
      FSorts[0] := sdDescending;
  end
  else begin
    Shift;
    FStack[0] := AFieldName;
    FSorts[0] := sdAscending;
  end;
end;

procedure TSortStack.Shift;
var
  i : integer;
begin
  for i := High(FStack) downto  Low(FStack)+1 do begin
    FStack[i] := FStack[i-1];
    FSorts[i] := FSorts[i-1];
  end;
end;

{ TUpdateDefinition }

procedure TSQLUpdate.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

function TSQLUpdate.GetSQL: string;
var
  i, j : integer;
//  Field : string;
begin
  if FKeyFields.Count = 0 then
    raise Exception.Create('Key fields not specified');
  if FUpdateFields.Count = 0 then
    raise Exception.Create('No fields to update');

  Result := Format('UPDATE %s SET '+#13,[FTableName]);

  for i := 0 to FUpdateFields.Count - 1 do
    if i = FUpdateFields.Count - 1 then
      Result := Result + Format('%s = :%0:s '+#13,[FUpdateFields[i].Name])
    else
      Result := Result + Format('%s = :%0:s, '+#13,[FUpdateFields[i].Name]);

  Result := Result + ' WHERE ';

  for j := 0 to FKeyFields.Count - 1 do
    if j = 0 then
      Result := Result + Format('%s = :%0:s '+#13,[FKeyFields[j].Name])
    else
      Result := Result + Format('AND %s = :%0:s '+#13,[FKeyFields[j].Name]);

end;

constructor TSQLUpdate.Create(const ATableName: string);
begin
  inherited Create;
  TableName := ATableName;
  FKeyFields := TFieldValueList.Create;
  FUpdateFields := TFieldValueList.Create;
end;

destructor TSQLUpdate.Destroy;
begin
  FKeyFields.Free;
  FUpdateFields.Free;
  inherited;
end;

procedure TSQLUpdate.SetCLOBParam(P : TParameter; const s : string);
var
  SS : TStringStream;
begin
  SS := TStringStream.Create(s);
  try
    P.LoadFromStream(SS, ftMemo);
  finally
    SS.Free;
  end;
end;

function TSQLUpdate.ExecuteQuery(Query: TADOQuery): integer;
var
  i, j : integer;
begin
  Result := -1;
  if Query = nil then Exit;
  with Query do begin
    if Active then Close;
    SQL.Clear;
    SQL.Add(GetSQL);
    Prepared := True;
    for i := 0 to FUpdateFields.Count - 1 do
      if FUpdateFields[i].Name = 'NOTES' then
        SetCLOBParam(Parameters.ParamByName(FUpdateFields[i].Name), VarToStr(FUpdateFields[i].Value))
      else
        Parameters.ParamByName(FUpdateFields[i].Name).Value := FUpdateFields[i].Value;
    for j := 0 to FKeyFields.Count - 1 do
      Parameters.ParamByName(FKeyFields[j].Name).Value := FKeyFields[j].Value;
    Result := ExecSQL;
  end;
end;

function TSQLUpdate.AddUpdateField(const AField: string;
  const AValue: Variant): integer;
begin
  Result := UpdateFields.Add(TFieldValue.Create(AField, AValue));
end;

function TSQLUpdate.AddKeyField(const AField: string;
  const AValue: Variant): integer;
begin
  Result := KeyFields.Add(TFieldValue.Create(AField, AValue));
end;

procedure TSQLUpdate.ShowQuery;
begin
//  ShowMessage(GetSQL);
end;

procedure TSQLUpdate.GetMetaData;
begin
  {}
end;

end.
