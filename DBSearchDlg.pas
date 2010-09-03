unit DBSearchDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExControls, JvComponent, JvArrowButton, ExtCtrls,
  VirtualTrees, ComCtrls, DB, ADODB, SQLTypes, DBSearchDlgThread;

type
  { Simulate  a list of a list of columns }
  TData = class(TObject)
  private
    FData : Variant;
  public
    property Data : Variant read FData write FData;
  end;

  TRowData = class(TList)
  private
    function GetData(index: integer): TData;
  public
    procedure Clear; override;
    property Data[index:integer]:TData read GetData; default;
  end;

  TRowDataList = class(TList)
  private
    function GetRowData(index: integer): TRowData;
  public
    procedure Clear; override;
    property RowData[index:integer]:TRowData read GetRowData; default;
  end;

  TSearchProperties = class;

  TFDBSearch = class(TForm)
    topPanel: TPanel;
    labelFilter: TLabel;
    JvArrowButton1: TJvArrowButton;
    Label1: TLabel;
    eSearchFilter: TEdit;
    cbSearchField: TComboBox;
    cbCaseSensitive: TCheckBox;
    SB: TStatusBar;
    panelBottom: TPanel;
    bCancel: TButton;
    bOK: TButton;
    vData: TVirtualStringTree;
  private
    FProps : TSearchProperties;
  public
    function InitSearch(Props : TSearchProperties):boolean;
    function FinalizeSearch(const Cancelled : boolean):boolean;
  end;

  TCustomWCCSearchDialog = class;

  TSearchField = class(TCollectionItem)
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

  TSearchFields = class(TCollection)
  private
    FSearchDialog : TCustomWCCSearchDialog;
    function GetItem(Index: Integer): TSearchField;
    procedure SetItem(Index: Integer; const Value: TSearchField);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(SearchDialog: TCustomWCCSearchDialog);
    function Add: TSearchField;
    function AddItem(Item: TSearchField; Index: Integer): TSearchField;
    function Insert(Index: Integer): TSearchField;
    property Items[Index: Integer]: TSearchField read GetItem write SetItem; default;
  end;

  { Class for passing parameters from component to dialog }
  TSearchProperties = class(TPersistent)
  private
    FDlg : TCustomWCCSearchDialog;
    FColumns: TSearchFields;
    FInitialValue: string;
    procedure SetInitialValue(const Value: string);
  public
    property Columns : TSearchFields read FColumns;
    property InitialValue : string read FInitialValue write SetInitialValue;
  end;

  TAfterExecute = procedure(const Cancelled : boolean; RowData : TRowData) of object;

  TCustomWCCSearchDialog = class(TComponent)
  private
    FDB: TADOConnection;
    FAutoUpperCase: boolean;
    FTableName: string;
    FResultRow : TRowData;
    FColumns: TSearchFields;
    FOnAfterExecute: TAfterExecute;
    FSearchProperties : TSearchProperties;
    FDBSearch : TFDBSearch;
    procedure SetDB(const Value: TADOConnection);
    procedure SetAutoUpperCase(const Value: boolean);
    procedure SetTableName(const Value: string);
    procedure SetColumns(const Value: TSearchFields);
    procedure SetOnAfterExecute(const Value: TAfterExecute);
  protected
//    procedure RefreshProps;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean; overload;
    function Execute(const InitialValue : string) : boolean; overload;
    property SearchProperties : TSearchProperties read FSearchProperties;
    { publishable }
    property AutoUpperCase : boolean read FAutoUpperCase write SetAutoUpperCase;
    property DB : TADOConnection read FDB write SetDB;
    property Columns : TSearchFields read FColumns write SetColumns;
    property TableName : string read FTableName write SetTableName;
    { Events }
    property OnAfterExecute : TAfterExecute read FOnAfterExecute write SetOnAfterExecute;
  end;

  TWCCSearchDialog = class(TCustomWCCSearchDialog)
  private
  protected
  public
  published
    { Properties }
    property AutoUpperCase;
    property Columns;
    property DB;
    property TableName;
    { Events }
    property OnAfterExecute;
  end;

//var
//  FDBSearch: TFDBSearch;

implementation

{$R *.dfm}

const
  ColWidthData = -1;
  ColWidthColName = -2;

{ TCustomWCCSearchDialog }

constructor TCustomWCCSearchDialog.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TSearchFields.Create(Self);
  // FDBSearch is nil until execute is called
  FSearchProperties := TSearchProperties.Create;
  FSearchProperties.InitialValue := '';
  FSearchProperties.FColumns := FColumns;
  FResultRow := TRowData.Create;
end;

destructor TCustomWCCSearchDialog.Destroy;
begin
  FResultRow.Free;
  FColumns.Free;
  FSearchProperties.Free;
  inherited;
end;

function TCustomWCCSearchDialog.Execute: boolean;
begin
  FDBSearch := TFDBSearch.Create(Self);
  with FDBSearch do
    try
      { Do setup }
//      RefreshProps;
      InitSearch(FSearchProperties);

      Result := ShowModal = mrOK;

      FinalizeSearch(Result);

      { Get anything extra before destruction }
      if Assigned(FOnAfterExecute) then
        FOnAfterExecute(Result, nil);  //todo setup rowdata
    finally
      Free;
    end;
end;

function TCustomWCCSearchDialog.Execute(const InitialValue: string): boolean;
begin
  FSearchProperties.InitialValue := InitialValue;
  Result := Execute;
end;

//procedure TCustomWCCSearchDialog.RefreshProps;
//begin
//  // set initial value
//  // setup columns
//  //FProps.Columns.Assign(FColumns);
//end;

procedure TCustomWCCSearchDialog.SetAutoUpperCase(const Value: boolean);
begin
  FAutoUpperCase := Value;
end;

procedure TCustomWCCSearchDialog.SetColumns(const Value: TSearchFields);
begin
  FColumns := Value;
end;

procedure TCustomWCCSearchDialog.SetDB(const Value: TADOConnection);
begin
  FDB.Assign(Value);
end;

procedure TCustomWCCSearchDialog.SetOnAfterExecute(const Value: TAfterExecute);
begin
  FOnAfterExecute := Value;
end;

procedure TCustomWCCSearchDialog.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

{ TRowData }

function TRowData.GetData(index: integer): TData;
begin
  Result := TData(Items[index]);
end;

procedure TRowData.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TData(Items[i]).Free;
  inherited;
end;

{ TRowDataList }

function TRowDataList.GetRowData(index: integer): TRowData;
begin
  Result := TRowData(Items[index]);
end;

procedure TRowDataList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TRowData(Items[i]).Free;
  inherited;
end;

{ TSearchField }

constructor TSearchField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  DisplayWidth := ColWidthColName;
  FVisible := True;
end;

procedure TSearchField.SetFilterField(const Value: boolean);
begin
  FFilterField := Value;
end;

procedure TSearchField.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TSearchField.Assign(Source: TPersistent);
begin
  if Source is TSearchField then
  begin
    ColumnName   := TSearchField(Source).ColumnName;
    FieldName    := TSearchField(Source).FieldName;
    DisplayWidth := TSearchField(Source).DisplayWidth;
    Visible      := TSearchField(Source).Visible;
    FilterField  := TSearchField(Source).FilterField;
    Predicate    := TSearchField(Source).Predicate;
    FilterValue  := TSearchField(Source).FilterValue;
    FieldType    := TSearchField(Source).FieldType;
  end
  else inherited Assign(Source);
end;

procedure TSearchField.SetPredicate(const Value: TWherePredicate);
begin
  FPredicate := Value;
end;

procedure TSearchField.SetFilterValue(const Value: Variant);
begin
  FFilterValue := Value;
end;

procedure TSearchField.SetDisplayWidth(const Value: integer);
begin
  FDisplayWidth := Value;
end;

procedure TSearchField.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TSearchField.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

function TSearchField.GetDisplayName: string;
begin
  if Length(FFieldName) > 0 then
    Result := FFieldName
  else
    Result := inherited GetDisplayName;
end;

procedure TSearchField.SetColumnName(const Value: string);
begin
  FColumnName := Value;
end;

{ TSearchFields }

constructor TSearchFields.Create(SearchDialog: TCustomWCCSearchDialog);
begin
  inherited Create(TSearchField);
  FSearchDialog := SearchDialog;
end;

function TSearchFields.Add: TSearchField;
begin
  Result := TSearchField(inherited Add);
end;

function TSearchFields.GetItem(Index: Integer): TSearchField;
begin
  Result := TSearchField(inherited GetItem(Index));
end;

procedure TSearchFields.SetItem(Index: Integer; const Value: TSearchField);
begin
  inherited SetItem(Index, Value);
end;

function TSearchFields.AddItem(Item: TSearchField;
  Index: Integer): TSearchField;
begin
  if Item = nil then
    Result := TSearchField.Create(Self)
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

procedure TSearchFields.Update(Item: TCollectionItem);
begin
  inherited;
  { do nothing? }
end;

function TSearchFields.Insert(Index: Integer): TSearchField;
begin
  Result := AddItem(nil, Index);
end;

function TSearchFields.GetOwner: TPersistent;
begin
  Result := FSearchDialog;
end;

{ TFDBSearch }

function TFDBSearch.FinalizeSearch(const Cancelled : boolean): boolean;
begin
  if Cancelled then begin
    { Any post dialog stuff whan cancelled? }
  end else begin
    { Read selected row }
  end;
end;

function TFDBSearch.InitSearch(Props : TSearchProperties):boolean;
var
  i, tw : integer;
  C : TVirtualTreeColumn;
begin
  FProps := Props;

  eSearchFilter.Text := FProps.InitialValue;

  { Setup columns }
  for i := 0 to FProps.Columns.Count - 1 do begin
    if FProps.Columns[i].Visible then begin
      C := vData.Header.Columns.Add;
      C.Text := FProps.Columns[i].ColumnName;
      if FProps.Columns[i].DisplayWidth = ColWidthColName then
        C.Width := vData.Canvas.TextWidth(C.Text) + 20
      else
        C.Width := FProps.Columns[i].DisplayWidth;
    end;
  end;
end;

{ TSearchProperties }

procedure TSearchProperties.SetInitialValue(const Value: string);
begin
  FInitialValue := Value;
end;

end.
