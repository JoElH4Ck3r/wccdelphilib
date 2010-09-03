unit DBSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ImgList, ADODB, SQLTypes,
  QueryThread,
  CustomWCCConnection,
  Menus, JvExControls, JvComponent, JvArrowButton, DB, XPStyleActnCtrls,
  ActnList, ActnMan, VirtualTrees,
  DBConnection;

const
  WM_AFTER_SHOW = WM_USER + 300; // custom message
  WM_AFTER_CREATE = WM_USER + 301; // custom message

  BIG_TABLES : array[0..1] of string = ('TLORDER', 'ODRSTAT');

type

  TCustomDBSearchDialog = class;
  TSearchColumns = class;

  TSearchColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FColumnName: string;
    FDisplayWidth: integer;
    FFilterField: boolean;
    FFieldType: TFieldType;
    FFilterValue: Variant;
    FPredicate: TWherePredicate;
    FVisible: boolean;
    procedure SetColumnName(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetDisplayWidth(const Value: integer);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetFilterField(const Value: boolean);
    procedure SetFilterValue(const Value: Variant);
    procedure SetPredicate(const Value: TWherePredicate);
    procedure SetVisible(const Value: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ColumnName : string read FColumnName write SetColumnName;
    property FieldName : string read FFieldName write SetFieldName;
    property DisplayWidth : integer read FDisplayWidth write SetDisplayWidth;
    property Visible : boolean read FVisible write SetVisible;
    {  All filtering }
    property FilterField : boolean read FFilterField write SetFilterField;
    property Predicate : TWherePredicate read FPredicate write SetPredicate;
    property FilterValue : Variant read FFilterValue write SetFilterValue;
    property FieldType : TFieldType read FFieldType write SetFieldType;
  end;

  TSearchColumns = class(TCollection)
  private
    FSearchDialog : TCustomDBSearchDialog;
    function GetItem(Index: Integer): TSearchColumn;
    procedure SetItem(Index: Integer; const Value: TSearchColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(SearchDialog: TCustomDBSearchDialog);
    function Add: TSearchColumn;
    function AddItem(Item: TSearchColumn; Index: Integer): TSearchColumn;
    function Insert(Index: Integer): TSearchColumn;
    property Items[Index: Integer]: TSearchColumn read GetItem write SetItem; default;
    {  }
    function TranslateColumnIndex(const Value : integer):integer;
  end;

  TfSearch = class(TForm)
    panelTop: TPanel;
    SmallImages: TImageList;
    panelBottom: TPanel;
    bCancel: TButton;
    bOK: TButton;
    Timer: TTimer;
    SB: TStatusBar;
    ViewPopup: TPopupMenu;
    iles1: TMenuItem;
    Icons1: TMenuItem;
    List1: TMenuItem;
    Details1: TMenuItem;
    LargeImages: TImageList;
    SaveDlg: TSaveDialog;
    Panel1: TPanel;
    labelFilter: TLabel;
    eSearchField: TEdit;
    JvArrowButton1: TJvArrowButton;
    Label1: TLabel;
    cbSearchField: TComboBox;
    cbCaseSensitive: TCheckBox;
    vData: TVirtualStringTree;
    Query: TADOQuery;
    procedure FormShow(Sender: TObject);
    procedure vDataDblClick(Sender: TObject);
    procedure vDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vDataInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vDataHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbCaseSensitiveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbSearchFieldCloseUp(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//    procedure SetListView(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure eSearchFieldKeyPress(Sender: TObject; var Key: Char);
    procedure rgFilterFieldClick(Sender: TObject);
    procedure eSearchFieldChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    SearchDialog : TCustomDBSearchDialog;
    ColumnToSort: Integer;
//    FQueryThread : TSQLQueryThread;
    FRowList : TSQLRowList;
    FLastSortColumn : string;
    FHaveMetaData : boolean;
    FNeedParams : boolean;
    FLastErrorMessage : string;
    FSortStack : TSortStack;
    FSearchChange : boolean;
    FSearchValue : string;
    FSearchField : string;
    FSearchCaseSensitive : boolean;
    FSearchPredicidate : TWherePredicate;
    FQueryExecutionTicks : DWord;
    procedure RefreshData;
    function GetSearchColumn:TSearchColumn;
    procedure SetRefreshTimer;
    procedure SelectData; overload;
    procedure SelectData(AField : string); overload;
//    procedure InitQueryThread;
//    procedure FinQueryThread;
//    procedure QueryThreadError(const ErrorMessage: string);
//    procedure ThreadMessage(const Msg: string);
    procedure SyncRowList;
    procedure SetThreadFilter;
    function GetColIndex(const AFieldName: string): integer;
    procedure DoQuery;
    function ReadMetaData: boolean;
    procedure QueryMessage(const Msg: string);
    function DoFetchQuery: boolean;
    function FieldsString: string;
    function WhereClause: string;
    procedure ClearRowList;
    procedure SetQueryParameters;
    function QueryFetch: TFetchOutcome;
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure WMAfterCreate(var Msg: TMessage); message WM_AFTER_CREATE;
  protected
    procedure AddEvent(Sender : TObject);
  public
    constructor Create(AOwner : TCustomDBSearchDialog; const InitialValue : string); reintroduce;
    destructor Destroy; override;
    procedure SaveDataToFile(const FileName : string);
    function GetExtraData(const AField : string): Variant;
  end;

  TFilterMethod = (fmStartsWith, fmContains);

  TNeedMoreData = procedure(RowData : TRowData) of object;

  TSearchDialogCloseEvent = procedure(Sender : TObject; SearchForm : TfSearch; ModalResult : TModalResult) of object;

  TCustomDBSearchDialog = class(TComponent)
  private
    FSearchForm : TfSearch;
    FTableName: string;
    FSearchColumns: TSearchColumns;
//    FDB: TADOConnection;
    FDB : TWCCConnection;
    FValue: Variant;
    FAutoUpperCase: boolean;
    FRefreshDelay: integer;
    FRecordBuffer: integer;
    FFilterMethod: TFilterMethod;
    FDebug: boolean;
    FReturnField: string;
    FOnBeforeExecute: TNotifyEvent;
    FOnNeedMoreData: TNeedMoreData;
    FOnBeforeClose: TSearchDialogCloseEvent;
    FDisableSorting: boolean;
    FValueRow: TRowData;
    FDialogCaption: string;
    FAllowCaseInsensitivity: boolean;
    FAllowEmptySearch: boolean;
    procedure SetTableName(const Value: string);
    procedure SetSearchColumns(const Value: TSearchColumns);
//    procedure SetDB(const Value: TADOConnection);
    procedure SetDB(const Value: TWCCConnection);
    procedure SetValue(const Value: Variant);
    procedure SetAutoUpperCase(const Value: boolean);
    procedure SetRefreshDelay(const Value: integer);
    procedure SetRecordBuffer(const Value: integer);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetDebug(const Value: boolean);
    procedure SetReturnField(const Value: string);
    procedure SetOnBeforeExecute(const Value: TNotifyEvent);
    procedure SetOnNeedMoreData(const Value: TNeedMoreData);
    {}
    procedure DoBeforeClose;
    procedure SetOnBeforeClose(const Value: TSearchDialogCloseEvent);
    procedure SetDisableSorting(const Value: boolean);
    procedure SetValueRow(const Value: TRowData);
    procedure SetAllowCaseInsensitivity(const Value: boolean);
    procedure SetAllowEmptySearch(const Value: boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean;
    property Value : Variant read FValue write SetValue;
    property ValueRow : TRowData read FValueRow write SetValueRow;
    function ValueAsString(const FieldName : string):string;
    function ValueAsInteger(const FieldName : string):integer;
    function ExtraData(const FieldName : string):Variant;
    { Publishable }
    property TableName : string read FTableName write SetTableName;
    property SearchColumns : TSearchColumns read FSearchColumns write SetSearchColumns;
//    property DB : TADOConnection read FDB write SetDB;
    property DB : TWCCConnection read FDB write SetDB;
    property AutoUpperCase : boolean read FAutoUpperCase write SetAutoUpperCase;
    property AllowCaseInsensitivity : boolean read FAllowCaseInsensitivity write SetAllowCaseInsensitivity;
    property AllowEmptySearch : boolean read FAllowEmptySearch write SetAllowEmptySearch;
    property FilterMethod : TFilterMethod read FFilterMethod write SetFilterMethod;
    property RefreshDelay : integer read FRefreshDelay write SetRefreshDelay;
    property RecordBuffer : integer read FRecordBuffer write SetRecordBuffer;
    property Debug : boolean read FDebug write SetDebug;
    property ReturnField : string read FReturnField write SetReturnField;
    property DialogCaption : string read FDialogCaption write FDialogCaption;
    { Not all fields can be sorted on ie CLOB, Memo, Text etc.                 }
    { This is crude but it totally disables any sorting                        }
    property DisableSorting : boolean read FDisableSorting write SetDisableSorting;
    { Events }
    property OnBeforeExecute : TNotifyEvent read FOnBeforeExecute write SetOnBeforeExecute;
    property OnBeforeClose : TSearchDialogCloseEvent read FOnBeforeClose write SetOnBeforeClose;
//    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write SetOnBeforeClose;
  end;

  TDBSearchDialog = class(TCustomDBSearchDialog)
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property AllowEmptySearch;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property TableName;
    property Debug;
    property ReturnField;
    property DisableSorting;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TDriverSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property AllowEmptySearch;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TTruckSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property AllowEmptySearch;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TTrailerSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property AllowEmptySearch;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TCargoSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property AllowEmptySearch;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TVendorSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AutoUpperCase;
    property AllowCaseInsensitivity;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TClientSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AllowCaseInsensitivity;
    property AutoUpperCase;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;

  TZoneSearchDialog = class(TCustomDBSearchDialog)
  public
    constructor Create(AOwner : TComponent); override;
  published
    { Properties }
    property AllowCaseInsensitivity;
    property AutoUpperCase;
    property DB;
    property FilterMethod;
    property RefreshDelay;
    property RecordBuffer;
    property SearchColumns;
    property ReturnField;
    property DialogCaption;
    { Events }
    property OnBeforeExecute;
    property OnBeforeClose;
  end;


  PSearchRec = ^TSearchRec;
  TSearchRec = record
    Data : TRowData;
  end;

var
  fSearch: TfSearch;

implementation

uses WCCUtil;

const
  ColWidthData = -1;
  ColWidthColName = -2;

  DefaultRecordBufferSize = 1000;
  DefaultRefreshDelay = 500; // Half a second

{$R *.dfm}

{ TSearchColumn }

procedure TSearchColumn.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TSearchColumn.SetColumnName(const Value: string);
begin
  FColumnName := Value;
end;

constructor TSearchColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  DisplayWidth := ColWidthColName;
  FVisible := True;
end;

function TSearchColumn.GetDisplayName: string;
begin
  if Length(FFieldName) > 0 then
    Result := FFieldName
  else
    Result := inherited GetDisplayName;
end;

procedure TSearchColumn.Assign(Source: TPersistent);
begin
  if Source is TSearchColumn then
  begin
    ColumnName := TSearchColumn(Source).ColumnName;
    FieldName  := TSearchColumn(Source).FieldName;
  end
  else inherited Assign(Source);
end;

procedure TSearchColumn.SetDisplayWidth(const Value: integer);
begin
  FDisplayWidth := Value;
end;

procedure TSearchColumn.SetFilterField(const Value: boolean);
begin
  FFilterField := Value;
end;

procedure TSearchColumn.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TSearchColumn.SetFilterValue(const Value: Variant);
begin
  FFilterValue := Value;
end;

procedure TSearchColumn.SetPredicate(const Value: TWherePredicate);
begin
  FPredicate := Value;
end;

procedure TSearchColumn.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

{ TSearchColumns }

function TSearchColumns.GetItem(Index: Integer): TSearchColumn;
begin
  Result := TSearchColumn(inherited GetItem(Index));
end;

procedure TSearchColumns.SetItem(Index: Integer; const Value: TSearchColumn);
begin
  inherited SetItem(Index, Value);
end;

constructor TSearchColumns.Create(SearchDialog: TCustomDBSearchDialog);
begin
  inherited Create(TSearchColumn);
  FSearchDialog := SearchDialog;
end;

function TSearchColumns.Add: TSearchColumn;
begin
  Result := TSearchColumn(inherited Add);
end;

function TSearchColumns.AddItem(Item: TSearchColumn;
  Index: Integer): TSearchColumn;
begin
  if Item = nil then
    Result := TSearchColumn.Create(Self)
  else
    Result := Item;
  if Assigned(Result) then
  begin
    Result.Collection := Self;
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

procedure TSearchColumns.Update(Item: TCollectionItem);
begin
  inherited;
  {}
end;

function TSearchColumns.Insert(Index: Integer): TSearchColumn;
begin
  Result := AddItem(nil, Index);
end;

function TSearchColumns.GetOwner: TPersistent;
begin
  Result := FSearchDialog;
end;

function TSearchColumns.TranslateColumnIndex(const Value: integer): integer;
var
  i, VisibleCount : integer;
begin
  { The display passes in the desired visible column index }
  { Here we search for the actual data column index based on }
  { the visible property }

  { Convert the absolute index to a visible column index }

  { The value in is the visible column index }
  VisibleCount := 0;
  for i := 0 to Count - 1 do begin
    if Items[i].Visible then
      Inc(VisibleCount);
    if VisibleCount = Value + 1 then begin
      Result := i;
      Break;
    end;
  end;

//  WCCTrace(Format('in: %d out: %d',[Value, Result]));
end;

{ TfSearch }

constructor TfSearch.Create(AOwner : TCustomDBSearchDialog; const InitialValue : string);
var
  i : integer;
//  LC : TListColumn;
  C : TVirtualTreeColumn;
  SC : TSearchColumn;
//  vFields : array of string;
begin
  inherited Create(AOwner);
  { Init vars }
  SearchDialog := AOwner;
  eSearchField.Text := InitialValue;
  FLastSortColumn := '';
  Caption := AOwner.DialogCaption;
  if not Assigned(SearchDialog.DB) then
    raise Exception.Create('No database component has been selected');
  if SearchDialog.SearchColumns.Count = 0 then
    raise Exception.Create('No columns defined');
  if Length(SearchDialog.TableName) = 0 then
    raise Exception.Create('No search table specified');
  { Setup initial state }
  Timer.Enabled := False;
  Timer.Interval := SearchDialog.RefreshDelay;
  FSortStack := TSortStack.Create(MAX_SORT_COL);

  { Case sensitivity }
  if not AOwner.AllowCaseInsensitivity then begin
    cbCaseSensitive.Checked := True;
    cbCaseSensitive.Enabled := False;
  end;

  


  { Filter Field Setup }
  cbSearchField.Items.Clear;
//  rgFilterField.Items.Clear;
//  rgFilterField.Columns := SearchDialog.SearchColumns.Count;
  vData.NodeDataSize := SizeOf(TSearchRec);
//  lvData.Columns.BeginUpdate;
  vData.Header.Columns.BeginUpdate;
  try
//    lvData.Columns.Clear;
    vData.Header.Columns.Clear;
    for i := 0 to SearchDialog.SearchColumns.Count -1 do begin
      if SearchDialog.SearchColumns[i].Visible then begin
        cbSearchField.Items.AddObject(SearchDialog.SearchColumns[i].ColumnName, TObject(i));
//        rgFilterField.Items.AddObject(SearchDialog.SearchColumns[i].ColumnName, TObject(i));
        { TListView }
//        LC := lvData.Columns.Add;
//        LC.Caption := SearchDialog.SearchColumns[i].ColumnName;
//        LC.Width := SearchDialog.SearchColumns[i].DisplayWidth;
        { TVirtualStringTree }
        C := vData.Header.Columns.Add;
        C.Text := SearchDialog.SearchColumns[i].ColumnName;
        C.Width := SearchDialog.SearchColumns[i].DisplayWidth;
      end;
    end;
  finally
    vData.Header.Columns.EndUpdate;
//    lvData.Columns.EndUpdate;
  end;
  if cbSearchField.Items.Count > 0 then
    cbSearchField.ItemIndex := 0
  else
    raise Exception.Create('Internal Error: No search columns?');
//  if rgFilterField.Items.Count > 0 then
//    rgFilterField.ItemIndex := 0;
  FRowList := TSQLRowList.Create(SearchDialog.TableName);
  FRowList.OnAdd := AddEvent;
  SyncRowList;
  { Asyncronously fetch records }
  //InitQueryThread;
  Query.Connection := AOwner.DB;
  SC := GetSearchColumn;
  FSortStack.Pop(SC.FieldName);
  FSearchChange := True;
  FHaveMetaData := False;

  SetThreadFilter;

//  DoQuery;
end;

procedure TfSearch.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TfSearch.WMAfterShow(var Msg: TMessage);
begin
  {}
end;

procedure TfSearch.WMAfterCreate(var Msg: TMessage);
begin
//  DoQuery;
end;

destructor TfSearch.Destroy;
begin
//  FinQueryThread;
  FSortStack.Free;
  FRowList.Free;
  inherited;
end;

procedure TfSearch.DoQuery;
var
  FetchComplete : boolean;
begin
  FetchComplete := False;
  if ReadMetaData then begin
    if not FetchComplete or FSearchChange then begin
      repeat
        FetchComplete := DoFetchQuery;
      until FetchComplete;
    end;
    vData.RootNodeCount := FRowList.Count;
    vData.FocusedNode := vData.GetFirstVisible;

    SB.Panels[0].Text := Format('Found %d in %f seconds',[FRowList.Count, FQueryExecutionTicks / 1000]);

  end;
end;

function TfSearch.ReadMetaData:boolean;
var
  i : integer;
  s : string;
begin
  QueryMessage('Reading meta data...');
  Result := False;
  if FHaveMetaData then begin
    Result := True;
    Exit;
  end
  else
  begin
    with Query do begin
      for i := 0 to FRowList.Fields.Count - 1 do begin
        if Active then Close;
        SQL.Clear;
        s := Format(SQLColDef,[
               UpperCase(SearchDialog.DB.SchemaName),
               UpperCase(FRowList.TableName),
               UpperCase(FRowList.Fields[i].Name)
               ]);
        SQL.Add(s);
        try
          Open;
          if not IsEmpty then begin
            FRowList.Fields[i].FieldTypeName := FieldByName('TYPENAME').AsString;
            FRowList.Fields[i].Scale := FieldByName('SCALE').AsInteger;
            FRowList.Fields[i].Length := FieldByName('LENGTH').AsInteger;
            if FieldByName('KEYSEQ').IsNull then
              FRowList.Fields[i].KeySequence := FieldByName('KEYSEQ').AsInteger;
          end
          else
            raise Exception.CreateFmt('QueryThread - Field %s.%s.%s not found',[
                    SearchDialog.DB.SchemaName,
                    FRowList.TableName,
                    FRowList.Fields[i].Name]);
          Close;
          Result := True;
        except
          raise;
//          on E : Exception do
//            FLastErrorMessage := E.Message;
        end;
      end;
    end;
    FHaveMetaData := Result;
  end;
  QueryMessage('');
end;

function TfSearch.DoFetchQuery:boolean;
var
  s : string;
  Mark : DWord;
begin
  QueryMessage('Querying Database...');
  with Query do begin
    try
      if Active then Close;
      SQL.Clear;
      { Start reading }
      case SearchDialog.DB.DatabaseServerType of
        dsDB2   :
          begin
            s := Format('SELECT %s FROM %s %s ',[FieldsString, FRowList.TableName, WhereClause]);
            if not FRowList.DisableSorting then
              s := s + Format('ORDER BY %s ',[FSortStack.SortFields]);
            s := s + Format('FETCH FIRST %d ROWS ONLY WITH UR',[SearchDialog.RecordBuffer]);
          end;
        dsMSSQL :
          begin
            s := Format('SELECT TOP(%d) %s FROM %s %s ',[SearchDialog.RecordBuffer, FieldsString, FRowList.TableName, WhereClause]);
            if not FRowList.DisableSorting then
              s := s + Format('ORDER BY %s ',[FSortStack.SortFields]);
          end;
      end;
//      if not FRowList.DisableSorting then begin
////        s := Format('SELECT %s FROM %s %s ORDER BY %s WITH UR',[
//        s := Format('SELECT %s FROM %s %s ORDER BY %s',[
//          FieldsString,
//          FRowList.TableName,
//          WhereClause,
//          FSortStack.SortFields])
//      end
//      else
//      begin
////        s := Format('SELECT %s FROM %s %s WITH UR',[
//        s := Format('SELECT %s FROM %s %s',[
//          FieldsString,
//          FRowList.TableName,
//          WhereClause]);
//      end;
      SQL.Add(s);
      try
        try
          ClearRowList;
          SetQueryParameters;

          { Benchmarking }
          FQueryExecutionTicks := 0;
          Mark := GetTickCount;

          Open;

          FQueryExecutionTicks := GetTickCount - Mark;
          
          case QueryFetch of
            foEof : Result := True;
            foChangeInterupt : Result := False;
            { Any exception message from the fetch would have already been displayed }
            foException : Result := True;
          end;
        except
          on E : Exception do begin
            FLastErrorMessage := E.Message;
            raise;
          end;
        end;
      finally
        Close;
      end;
    except
      on E : Exception do begin
        Result := True;
        raise;
//        FLastErrorMessage := E.Message;
      end;
    end;
  end;
  QueryMessage('');
end;

function TfSearch.QueryFetch:TFetchOutcome;
var
  i, idx, rowidx, cnt : integer;
begin
  QueryMessage('Reading records...');
  try
    if not Query.Active then Exit;
    while not Query.Eof do begin
      if FSearchChange then begin
        Result := foChangeInterupt;
        Break;
      end;
      Inc(cnt);
      idx := FRowList.Add(TRowData.Create(FRowList.Fields.Count));
      rowidx := 0;
      for i := 0 to FRowList.Fields.Count - 1 do begin
        if FRowList.Fields[i].FieldType = ftMemo then
          FRowList[idx].Data[i] := Query.FieldByName(FRowList.Fields[i].Name).AsString
        else
          FRowList[idx].Data[i] := Query.FieldByName(FRowList.Fields[i].Name).Value;
      end;
      Query.Next;
    end;
    if Query.Eof then
      Result := foEof;
  except
    on E : Exception do begin
      FLastErrorMessage := E.Message;
      Result := foEof;
    end;
  end;
  QueryMessage('');
end;

procedure TfSearch.ClearRowList;
begin
  if FRowList <> nil then
    FRowList.Clear;
end;

function TfSearch.FieldsString: string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to FRowList.Fields.Count - 1 do begin
    if i = 0 then
      Result := Result + FRowList.Fields[i].Name
    else
      Result := Result + ',' + FRowList.Fields[i].Name;
  end;
end;

function TfSearch.WhereClause: string;
var
  i, WhereCount : integer;
begin
  WhereCount := 0;
  Result := '';
  { First add any design time conditions }
  for i := 0 to FRowList.Fields.Count - 1 do
    if FRowList.Fields[i].Filtered then begin
      Inc(WhereCount);
      Result := iif_str(WhereCount > 1, 'AND ','WHERE ');
//      if WhereCount > 1 then
//        Result := 'WHERE '
//      else
//        Result := 'AND ';
      Result := Format('%s %s %s :%s', [
                  Result,
                  FRowList.Fields[i].Name,
                  WherePredicates[FRowList.Fields[i].WherePredicate],
                  FRowList.Fields[i].Name
                  ]);
      if not FNeedParams then
        FNeedParams := True;
    end;
  { Second add filter if it exists }
  if FSearchChange then begin
//    if Length(VarToStr(FSearchValue)) > 0 then begin

    { Do not allow blank search criteria }
    if ((Length(FSearchValue) = 0) and (not SearchDialog.AllowEmptySearch)) then
      raise Exception.Create('Empty search criteria not allowed. Please type something into the "Search String" box.');
      //FSearchValue := '~';
      
    if Length(FSearchValue) > 0 then begin
      { Add condition }
      if Length(Result) > 0 then
        Result := Format('%s AND %s %s :%s',[
                    Result,
                    {TODO: Handle CLOBs more efficiently  }
                    iif_str(cbCaseSensitive.Checked,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),
                    //iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),

                    WherePredicates[FSearchPredicidate],
                    FSearchField])
      else
        Result := Format('WHERE %s %s :%s',[
                    {TODO: Handle CLOBs more efficiently  }
                    iif_str(cbCaseSensitive.Checked,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),
                    //iif_str(FSearchCaseSensitive,FSearchField,Format('UPPER(CAST(%s AS VARCHAR(1000)))',[FSearchField])),
                    WherePredicates[FSearchPredicidate],
                    FSearchField]);
      { Notify we need parameters }
      if not FNeedParams then
        FNeedParams := True;
    end;
    { Reset Changed flag }
    FSearchChange := False;
  end;
end;

procedure TfSearch.SetQueryParameters;
var
  i : integer;
begin
  if FNeedParams then begin
    with Query do begin
      try
        Prepared := True;
        { User input filter }
        if Length(VarToStr(FSearchValue)) > 0 then
          Parameters.ParamByName(FSearchField).Value := FSearchValue;

        { Design time filters }
        for i := 0 to FRowList.Fields.Count - 1 do begin
          if FRowList.Fields[i].Filtered then begin
            Parameters.ParamByName(FRowList.Fields[i].Name).Value := FRowList.Fields[i].FilterValue;
          end;
        end;
        {
        for i := 0 to Parameters.Count - 1 do begin
          // Deal with this later
        end;
        }
        FNeedParams := False;
      except
        on E : Exception do
          FLastErrorMessage := E.Message;
      end;
    end;
  end;
end;

//procedure TfSearch.SetListView(Sender: TObject);
//begin
//  { TListView }
////  if Sender is TMenuItem then begin
////    lvData.ViewStyle := TViewStyle(TMenuItem(Sender).Tag);
////    TMenuItem(Sender).Checked := True;
////  end;
//  { TVirtualTree }
//  // Do nothing, only one view style
//end;

procedure TfSearch.SyncRowList;
var
  i : integer;
  vFields : array of TFieldQueryInfo;
begin
  SetLength(vFields, SearchDialog.SearchColumns.Count);
  for i := 0 to SearchDialog.SearchColumns.Count -1 do begin
    vFields[i].FieldName       := SearchDialog.SearchColumns[i].FieldName;
    vFields[i].Visible         := SearchDialog.SearchColumns[i].Visible;
    vFields[i].Filter          := SearchDialog.SearchColumns[i].FilterField;
    vFields[i].FilterPredicate := SearchDialog.SearchColumns[i].Predicate;
    vFields[i].FilterValue     := SearchDialog.SearchColumns[i].FilterValue;
  end;
  FRowList.SetFields(vFields);
end;

//procedure TfSearch.QueryThreadError(const ErrorMessage : string);
//begin
//  ErrorMsg(ErrorMessage);
//end;

//procedure TfSearch.InitQueryThread;
//var
//  DP : TDelimParser;
//  SC : TSearchColumn;
//begin
//  DP := TDelimParser.Create(SearchDialog.DB.ConnectionString,';');
//  try
//    FRowList.DisableSorting := SearchDialog.DisableSorting;
//    FQueryThread := TSQLQueryThread.Create(
//                      DP.Elements['User ID'],
//                      DP.Elements['Password'],
//                      DP.Elements['Data Source'],
//                      FRowList,
//                      SearchDialog.RecordBuffer);
//    FQueryThread.Debug := SearchDialog.Debug;
//    FQueryThread.OnError := QueryThreadError;
//    FQueryThread.OnMessage := ThreadMessage;
//    FQueryThread.FreeOnTerminate := False;
//    { Add initial sorting }
//    SC := GetSearchColumn;
//    FLastSortColumn := SC.FieldName;
//    FQueryThread.AddSearchField(SC.FieldName);
//    SetThreadFilter;
//    FQueryThread.Resume;
//  finally
//    DP.Free;
//  end;
//end;

procedure TfSearch.SetThreadFilter;
var
  s : string;
  WP: TWherePredicate;
  SC : TSearchColumn;
  FilterValue : string;
begin
  { Setup the filter value and predicate }
  { When the seach column field type is unknown assume the search field is character }
  SC := GetSearchColumn;
  WP := wpLike;
  FilterValue := '';
  if Length(eSearchField.Text) > 0 then begin

    FilterValue := iif_str(SearchDialog.FilterMethod = fmContains,'%'+eSearchField.Text+'%',eSearchField.Text+'%');
    { Handle other types if set }
    { integer, date, timestamp }
    if SC.FieldType = ftInteger then begin
      { for integers ignore FilterMethod }
      WP := wpGreaterThanEqualTo;
      FilterValue := eSearchField.Text; // We assume the user has typed a valid number
    end
    else if SC.FieldType = ftDateTime then begin
      { TODO }
    end;
  end;
  if not cbCaseSensitive.Checked then
    FilterValue := UpperCase(FilterValue);

  FSearchField := SC.FieldName;
  FSearchValue := FilterValue;
  FSearchPredicidate := WP;//SearchPredicate;
//  FSearchCaseSensitive := CaseSensitive;

  FSearchCaseSensitive := not cbCaseSensitive.Checked;

  FSearchChange := True;

  DoQuery;
end;

procedure TfSearch.eSearchFieldKeyPress(Sender: TObject; var Key: Char);
begin
//  if SearchDialog.AutoUpperCase then

//  if not cbCaseSensitive.Checked then
//    Key := UpperChar(Key);
end;

function TfSearch.GetSearchColumn: TSearchColumn;
var
  idx : integer;
begin
  { Get the field selected in rgFilterField  }
  idx := integer(cbSearchField.Items.Objects[cbSearchField.ItemIndex]);
  Result := SearchDialog.SearchColumns.Items[idx];
end;

//procedure TfSearch.ThreadMessage(const Msg : string);
//begin
//  SB.Panels[1].Text := Msg;
//end;

//procedure TfSearch.FinQueryThread;
//begin
//  try
//    if FQueryThread <> nil then begin
//      if not FQueryThread.Terminated then begin
//        FQueryThread.Terminate;
//        FQueryThread.WaitFor;
//      end;
//      FQueryThread.Free;
//    end;
//  except
//    on E : Exception do
//      WCCTrace('TfSearch.FinQueryThread.Exception:'+E.Message);
//  end;
//end;

procedure TfSearch.RefreshData;
begin
  Screen.Cursor := crHourGlass;
  try
    SetThreadFilter;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfSearch.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;

  vData.BeginUpdate;
  try
    vData.Clear;
  finally
    vData.EndUpdate;
  end;

  RefreshData;
end;

procedure TfSearch.eSearchFieldChange(Sender: TObject);
begin
  SetRefreshTimer;
end;

procedure TfSearch.cbCaseSensitiveClick(Sender: TObject);
begin
  SetRefreshTimer;
end;

procedure TfSearch.rgFilterFieldClick(Sender: TObject);
begin
//  SetRefreshTimer;
end;

procedure TfSearch.cbSearchFieldCloseUp(Sender: TObject);
begin
  SetRefreshTimer;
end;

procedure TfSearch.SetRefreshTimer;
begin
  if Visible then begin
    if not Timer.Enabled then
      Timer.Enabled := True
    else begin
      Timer.Enabled := False;
      Timer.Enabled := True;
    end;
  end;
end;

//procedure TfSearch.lvDataCompare(Sender: TObject; Item1,
//  Item2: TListItem; Data: Integer; var Compare: Integer);
////var
////  ix: Integer;
//begin
////  if ColumnToSort = 0 then
////    Compare := CompareText(Item1.Caption,Item2.Caption)
////  else begin
////   ix := ColumnToSort - 1;
////   Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);
////  end;
//end;

function TfSearch.GetColIndex(const AFieldName : string):integer;
var
  i : integer;
  Found : boolean;
begin
  Result := -1;
  Found := False;
  for i := 0 to FRowList.Fields.Count - 1 do begin
    Found := Trim(FRowList.Fields[i].Name) = Trim(AFieldName);
    if Found then begin
      Result := i;
      Break;
    end;
  end;
end;

//procedure TfSearch.lvDataDblClick(Sender: TObject);
//begin
//  SelectData;
//end;

procedure TfSearch.bOKClick(Sender: TObject);
begin
  SelectData;
end;

procedure TfSearch.SaveDataToFile(const FileName: string);
begin
  Screen.Cursor := crHourGlass;
  try
    FRowList.SaveToFile(FileName);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { About to close, last chance to get any "Extra Data" hint, hint }
  SearchDialog.DoBeforeClose; // private member of SearchDialog
end;

procedure TfSearch.QueryMessage(const Msg: string);
begin
  SB.Panels[1].Text := Msg;
end;

{ TCustomDBSearchDialog }

constructor TCustomDBSearchDialog.Create(AOwner: TComponent);
begin
  inherited;
  { init variables }
  FSearchColumns := TSearchColumns.Create(Self);
  FValueRow := TRowData.Create(0);
  { Set default state }
  FAutoUpperCase := True;
  FRefreshDelay := DefaultRefreshDelay;
  FRecordBuffer := DefaultRecordBufferSize;
  FFilterMethod := fmStartsWith;
  FDisableSorting := False;
  FAllowCaseInsensitivity := True;
  FAllowEmptySearch := True;
end;

destructor TCustomDBSearchDialog.Destroy;
begin
  FValueRow.Free;
  FSearchColumns.Free;
  inherited;
end;

procedure TCustomDBSearchDialog.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

function TCustomDBSearchDialog.ExtraData(const FieldName: string): Variant;
begin
  if FSearchForm <> nil then
    Result := FSearchForm.GetExtraData(FieldName)
  else
    raise Exception.Create('Search form not available');
end;

procedure TCustomDBSearchDialog.SetSearchColumns(const Value: TSearchColumns);
begin
  FSearchColumns := Value;
end;

procedure TCustomDBSearchDialog.SetDB(const Value: TWCCConnection);
begin
  FDB := Value;
end;

function TCustomDBSearchDialog.Execute: boolean;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

  FSearchForm := TfSearch.Create(Self, Value);
  try
    Result := FSearchForm.ShowModal = mrOK;
  finally
    FSearchForm.Free;
  end;

//  with TfSearch.Create(Self, Value) do try
//    Result := ShowModal = mrOK;
//  finally
//    Free;
//  end;
end;

procedure TCustomDBSearchDialog.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TCustomDBSearchDialog.SetAllowCaseInsensitivity(const Value: boolean);
begin
  FAllowCaseInsensitivity := Value;
end;

procedure TCustomDBSearchDialog.SetAllowEmptySearch(const Value: boolean);
begin
  FAllowEmptySearch := Value;
end;

procedure TCustomDBSearchDialog.SetAutoUpperCase(const Value: boolean);
begin
  FAutoUpperCase := Value;
end;

procedure TCustomDBSearchDialog.SetRefreshDelay(const Value: integer);
begin
  FRefreshDelay := Value;
end;

procedure TCustomDBSearchDialog.SetRecordBuffer(const Value: integer);
begin
  FRecordBuffer := Value;
end;

procedure TCustomDBSearchDialog.SetFilterMethod(const Value: TFilterMethod);
begin
  FFilterMethod := Value;
end;

procedure TCustomDBSearchDialog.SetDebug(const Value: boolean);
begin
  FDebug := Value;
end;

procedure TCustomDBSearchDialog.SetReturnField(const Value: string);
begin
  FReturnField := Value;
end;

procedure TCustomDBSearchDialog.SetOnBeforeExecute(const Value: TNotifyEvent);
begin
  FOnBeforeExecute := Value;
end;

procedure TCustomDBSearchDialog.SetOnNeedMoreData(const Value: TNeedMoreData);
begin
  FOnNeedMoreData := Value;
end;

procedure TCustomDBSearchDialog.DoBeforeClose;
begin
  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, FSearchForm, FSearchForm.ModalResult);
end;

procedure TCustomDBSearchDialog.SetOnBeforeClose(const Value: TSearchDialogCloseEvent);
begin
  FOnBeforeClose := Value;
end;

procedure TCustomDBSearchDialog.SetDisableSorting(const Value: boolean);
begin
  FDisableSorting := Value;
end;

procedure TCustomDBSearchDialog.SetValueRow(const Value: TRowData);
begin
  FValueRow.Assign(Value);
end;

function TCustomDBSearchDialog.ValueAsString(const FieldName: string): string;
//var
//  i : integer;
begin
  raise Exception.Create('ValueAsString not yet ready for prime time');
//  for i := 0 to FSearchColumns.Count - 1 do
//    if CompareText(FieldName, FSearchColumns[i].FieldName) = 0 then begin
//      Result := FValueRow[i].AsString;
//      Break;
//    end;
end;

function TCustomDBSearchDialog.ValueAsInteger(const FieldName: string): integer;
//var
//  i : integer;
begin
  raise Exception.Create('ValueAsInteger not yet ready for prime time');
//  for i := 0 to FSearchColumns.Count - 1 do
//    if CompareText(FieldName, FSearchColumns[i].FieldName) = 0 then begin
//      Result := FValueRow[i];
//      Break;
//    end;
end;

{ TDriverSearchDialog }

constructor TDriverSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'DRIVER';
  FReturnField := 'DRIVER_ID';
  FDialogCaption := 'Driver Search';
  { DRIVER_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'DRIVER_ID';
  SC.ColumnName := 'Driver Code';
  SC.DisplayWidth := 100;
  { NAME }
  SC := FSearchColumns.Add;
  SC.FieldName := 'NAME';
  SC.ColumnName := 'Name';
  SC.DisplayWidth := 100;
  { CITY }
  SC := FSearchColumns.Add;
  SC.FieldName := 'CITY';
  SC.ColumnName := 'City';
  SC.DisplayWidth := 100;
  { PROVINCE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'PROVINCE';
  SC.ColumnName := 'State/Prov';
  SC.DisplayWidth := 100;
  { PHONE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'PHONE';
  SC.ColumnName := 'Phone';
  SC.DisplayWidth := 100;
end;

{ TTruckSearchDialog }

constructor TTruckSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'PUNIT';
  FReturnField := 'UNIT_ID';
  FDialogCaption := 'Truck Search';
  { PUNIT_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'UNIT_ID';
  SC.ColumnName := 'Truck Code';
  SC.DisplayWidth := 100;
  { MAKE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'MAKE';
  SC.ColumnName := 'Make';
  SC.DisplayWidth := 100;
  { YEAR }
  SC := FSearchColumns.Add;
  SC.FieldName := 'YEAR';
  SC.ColumnName := 'Year';
  SC.DisplayWidth := 100;
end;

{ TTrailerSearchDialog }

constructor TTrailerSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'TRAILER';
  FReturnField := 'TRAILER_ID';
  FDialogCaption := 'Trailer Search';
  { TRAILER_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'TRAILER_ID';
  SC.ColumnName := 'Trailer Code';
  SC.DisplayWidth := 100;
  { TYPE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'TYPE';
  SC.ColumnName := 'Type';
  SC.DisplayWidth := 100;
  { YEAR }
  SC := FSearchColumns.Add;
  SC.FieldName := 'YEAR';
  SC.ColumnName := 'Year';
  SC.DisplayWidth := 100;
end;

{ TCargoSearchDialog }

constructor TCargoSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'TLORDER';
  FReturnField := 'DETAIL_LINE_ID';
  FDialogCaption := 'Freight Search';
  { BILL_NUMBER }
  SC := FSearchColumns.Add;
  SC.FieldName := 'DETAIL_LINE_ID';
  SC.ColumnName := '';
  SC.DisplayWidth := 100;
  SC.Visible := False;
  { BILL_NUMBER }
  SC := FSearchColumns.Add;
  SC.FieldName := 'BILL_NUMBER';
  SC.ColumnName := 'Bill Number';
  SC.DisplayWidth := 100;
  { CALLER }
  SC := FSearchColumns.Add;
  SC.FieldName := 'CUSTOMER';
  SC.ColumnName := 'Caller';
  SC.DisplayWidth := 100;
  { SHIPPER }
  SC := FSearchColumns.Add;
  SC.FieldName := 'ORIGIN';
  SC.ColumnName := 'Shipper';
  SC.DisplayWidth := 100;
  { CONSIGNEE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'DESTINATION';
  SC.ColumnName := 'Consignee';
  SC.DisplayWidth := 100;
end;

{ TVendorSearchDialog }

constructor TVendorSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'VENDOR';
  FReturnField := 'VENDOR_ID';
  FDialogCaption := 'Vendor Search';
  { VENDOR_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'VENDOR_ID';
  SC.ColumnName := 'Vendor Code';
  SC.DisplayWidth := 100;
  { NAME }
  SC := FSearchColumns.Add;
  SC.FieldName := 'NAME';
  SC.ColumnName := 'Name';
  SC.DisplayWidth := 100;
  { CITY }
  SC := FSearchColumns.Add;
  SC.FieldName := 'CITY';
  SC.ColumnName := 'City';
  SC.DisplayWidth := 100;
  { BUSINESS_PHONE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'BUSINESS_PHONE';
  SC.ColumnName := 'Phone Number';
  SC.DisplayWidth := 100;
  { POSTAL_CODE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'POSTAL_CODE';
  SC.ColumnName := 'Postal Code';
  SC.DisplayWidth := 100;
end;

{ TClientSearchDialog }

constructor TClientSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'CLIENT';
  FReturnField := 'CLIENT_ID';
  FDialogCaption := 'Client Search';
  { CLIENT_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'CLIENT_ID';
  SC.ColumnName := 'Client Code';
  SC.DisplayWidth := 100;
  { NAME }
  SC := FSearchColumns.Add;
  SC.FieldName := 'NAME';
  SC.ColumnName := 'Name';
  SC.DisplayWidth := 200;
  { CITY }
  SC := FSearchColumns.Add;
  SC.FieldName := 'CITY';
  SC.ColumnName := 'City';
  SC.DisplayWidth := 100;
  { BUSINESS_PHONE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'BUSINESS_PHONE';
  SC.ColumnName := 'Phone Number';
  SC.DisplayWidth := 100;
  { POSTAL_CODE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'POSTAL_CODE';
  SC.ColumnName := 'Postal Code';
  SC.DisplayWidth := 100;
end;

{ TZoneSearchDialog }

constructor TZoneSearchDialog.Create(AOwner: TComponent);
var
  SC : TSearchColumn;
begin
  inherited;
  FTableName := 'ZONE';
  FReturnField := 'ZONE_ID';
  FDialogCaption := 'Zone Search';
  { ZONE_ID }
  SC := FSearchColumns.Add;
  SC.FieldName := 'ZONE_ID';
  SC.ColumnName := 'Zone Code';
  SC.DisplayWidth := 100;
  { SHORT_DESCRIPTION }
  SC := FSearchColumns.Add;
  SC.FieldName := 'SHORT_DESCRIPTION';
  SC.ColumnName := 'Description';
  SC.DisplayWidth := 200;
  { PARENT_ZONE }
  SC := FSearchColumns.Add;
  SC.FieldName := 'PARENT_ZONE';
  SC.ColumnName := 'Parent Zone';
  SC.DisplayWidth := 200;
end;

procedure TfSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  if (ssCtrl in Shift) then
//    WCCTrace(Format('Key: %d', [Key]));
  if (ssCtrl in Shift) and (Key = 83) then
    if SaveDlg.Execute then
      SaveDataToFile(SaveDlg.FileName);
end;

//procedure TfSearch.lvDataColumnClick(Sender: TObject;
//  Column: TListColumn);
//var
//  SC : TSearchColumn;
//  idx : integer;
//  SortColumn : string;
//begin
//  { TODO: Sorting }
//  if Column.Index > cbSearchField.Items.Count then Exit;
//  idx := integer(cbSearchField.Items.Objects[Column.Index]);
//  SC := SearchDialog.SearchColumns.Items[idx];
//  { Find then field associated with the column that was clicked }
////  if SC.FieldName = FLastSortColumn then
////    SortColumn := SC.FieldName + ' DESC'
////  else
////    SortColumn := SC.FieldName;
//  FQueryThread.AddSearchField(SC.FieldName);
////  FLastSortColumn := SC.FieldName;
////  ColumnToSort := Column.Index;
////  (Sender as TCustomListView).AlphaSort;
//end;

procedure TfSearch.vDataHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { Sorting }
end;

procedure TfSearch.AddEvent(Sender: TObject);
var
  vCount, vRecordCount : integer;
begin
  { This is the event that sends the data back to the display }

  if Sender is TSQLRowList then begin

    vCount := TSQLRowList(Sender).Count;
    vRecordCount := TSQLRowList(Sender).RecordCount;

//    WCCTrace(Format('Records %d of %d',[vCount, vRecordCount]));

    { TListView }
//    lvData.Items.Count := vCount;
//    lvData.Repaint;

    { TVirtualTree }
    with vData do begin
      BeginUpdate;
      try
        Clear;
        RootNodeCount := vCount;
      finally
        EndUpdate;
      end;
    end;

//    SB.Panels[0].Text := Format('Records %d of %d',[TSQLRowList(Sender).Count, TSQLRowList(Sender).RecordCount]);
    SB.Panels[0].Text := Format('Records %d of %d',[vCount, vRecordCount]);
  end;

  Application.ProcessMessages;
end;

//procedure TfSearch.lvDataData(Sender: TObject; Item: TListItem);
//var
//  i, RowIdx : integer;
//  DoneFirst, DoCaption : boolean;
//begin
//  if (Item.Index >= FRowList.Count) then Exit;
//  RowIdx := 0;
//  DoneFirst := False;
//  DoCaption := True;
//  if lvData.ViewStyle = vsReport then begin
//    for i := 0 to FRowList.Fields.Count - 1 do begin
//      { Danger!!! do we get out of sync here??? }
//      if FRowList.Fields[i].Visible then begin
//        try
//          if DoCaption then begin
//            Item.Caption := FRowList[Item.Index].AsString(i);
//            DoCaption := False;
//          end
//          else begin
//            Item.SubItems.Add(FRowList[Item.Index].AsString(i));
//          end;
//        except
//        end;
//      end;
//    end;
//  end
//  else
//    {TODO: find first visible field }
//    Item.Caption := FRowList[Item.Index].Data[0];
//  Item.ImageIndex := 0;
////  if Item.Index = 0 then
////    lvData.Selected := Item;
//end;

//procedure TfSearch.lvDataDataFind(Sender: TObject; Find: TItemFind;
//  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
//  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
//  var Index: Integer);
//var
//  I: Integer;
//  Found: Boolean;
//begin
//  I := StartIndex;
//  if (Find = ifExactString) or (Find = ifPartialString) then
//  begin
//    repeat
//      if (I = FRowList.Count-1) then
//        if Wrap then I := 0 else Exit;
//      Found := Pos(UpperCase(FindString), UpperCase(VarToStr(FRowList[I].Data[0]))) = 1;
//      Inc(I);
//    until Found or (I = StartIndex);
//    if Found then Index := I-1;
//  end;
//end;

//procedure TfSearch.lvDataDataHint(Sender: TObject; StartIndex,
//  EndIndex: Integer);
//begin
//  {}
//end;

procedure TfSearch.vDataInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  NodeData : PSearchRec;
begin
  NodeData := Sender.GetNodeData(Node);
  NodeData^.Data := FRowList[Node.Index];
end;

procedure TfSearch.vDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  NodeData : PSearchRec;
begin
  NodeData := Sender.GetNodeData(Node);
  try
    { Need to transpose the requested column to a visible column }

//    CellText := NodeData^.Data[SearchDialog.SearchColumns.TranslateColumnIndex(Column)];

    CellText := NodeData^.Data.AsString(SearchDialog.SearchColumns.TranslateColumnIndex(Column));

  except
//    on E : Exception do
//      WCCTrace(
//        Format('Handled Exception [vDataGetText] col: %d idx: %d',[
//        Column,
//        Node.Index
//        ]));
  end;
end;

function TfSearch.GetExtraData(const AField: string): Variant;
var
//  LI : TListItem;
  i, ColIdx : integer;
  NodeData : PSearchRec;
begin
  raise Exception.Create('Extra data functionality depreciated');
  { TODO: VTree migration }
//  LI := lvData.Selected;
//  if LI <> nil then begin
//    ColIdx := GetColIndex(AField);
//    if ColIdx = -1 then
//      ColIdx := 0;
//    Result := FRowList.Row[LI.Index].Data[ColIdx];
//  end
//  else
//    ErrorMsg('You have not selected anything');
end;

procedure TfSearch.SelectData;
begin
  SelectData(SearchDialog.ReturnField);
end;

procedure TfSearch.vDataDblClick(Sender: TObject);
begin
  SelectData;
end;

procedure TfSearch.SelectData(AField : string);
var
//  LI : TListItem;
  i, ColIdx : integer;
  NodeData : PSearchRec;
begin
  with vData do begin
    if FocusedNode = nil then begin
      ErrorMsg('You have not selected anything');
      ModalResult := mrNone;
    end else begin
      NodeData := GetNodeData(FocusedNode);
      if NodeData^.Data = nil then begin
        ErrorMsg('Error reading selection');
        ModalResult := mrNone;
      end;
      { Get selection }
      ColIdx := GetColIndex(AField);
      if ColIdx = -1 then
        ColIdx := 0;
      SearchDialog.Value := FRowList.Row[FocusedNode.Index].Data[ColIdx];

      { Better support for accessing more row data }
      SearchDialog.ValueRow.FieldCount := FRowList.Row[FocusedNode.Index].FieldCount;
      SearchDialog.ValueRow.Assign(FRowList.Row[FocusedNode.Index]);

      ModalResult := mrOK;
    end;
  end;

  { TODO: vTree Migration }
//  LI := lvData.Selected;
//  if LI <> nil then begin
//    ColIdx := GetColIndex(AField);
//    if ColIdx = -1 then
//      ColIdx := 0;
//      //raise Exception.Create('TfSearch.SelectData - Cannot find return field');
//    SearchDialog.Value := FRowList.Row[LI.Index].Data[ColIdx];
//    SearchDialog.ValueRow.FieldCount := FRowList.Row[LI.Index].FieldCount;
//    SearchDialog.ValueRow.Assign(FRowList.Row[LI.Index]);
////    SearchDialog.Value := LI.Caption;
//    ModalResult := mrOK;
//  end
//  else
//  begin
//    ErrorMsg('You have not selected anything');
//    ModalResult := mrNone;
//  end;
end;

end.
