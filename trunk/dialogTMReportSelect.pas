unit dialogTMReportSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ComCtrls, ToolWin, classTMReports,
  ADODB, ActnList, Menus, WCCUtil, UCrpe32;

type
  TWherePredicate  = (wpEquals, wpNotEqualTo, wpGreaterThan, wpLessThan,
    wpGreaterThanEqualTo, wpLessThanEqualTo);

const
  WherePredicates : array[TWherePredicate] of string[2] = ('=','<>','>','<','>=','<=');

  DefaultWindowTitle = 'West Coast Code';

type
  TCustomReportDialog = class;
  TSQLSubs = class;

  TOnSQLSubValue = procedure(var Value : Variant) of object;
  TOnSetParameters = procedure(ParamFields : TCrpeParamFields) of object;

  TDirection = (dAscending, dDescending);

  TSQLOrderField = class(TCollectionItem)
  private
    FEnabled: boolean;
    FDirection: TDirection;
    FFieldName: string;
    FValue: Variant;
    procedure SetDirection(const Value: TDirection);
    procedure SetEnabled(const Value: boolean);
    procedure SetFieldName(const Value: string);
    procedure SetValue(const Value: Variant);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Value : Variant read FValue write SetValue;
  published
    property FieldName : string read FFieldName write SetFieldName;
    property Direction : TDirection read FDirection write SetDirection;
    property Enabled : boolean read FEnabled write SetEnabled;
  end;

  TSQLOrderFields = class(TCollection)
  private
    FReportDialog : TCustomReportDialog;
    function GetItem(Index: Integer): TSQLOrderField;
    procedure SetItem(Index: Integer; const Value: TSQLOrderField);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ReportDialog: TCustomReportDialog);
    function Add: TSQLOrderField;
    function AddItem(Item: TSQLOrderField; Index: Integer): TSQLOrderField;
    function Insert(Index: Integer): TSQLOrderField;
    property Items[Index: Integer]: TSQLOrderField read GetItem write SetItem; default;
  end;


  TSQLSub = class(TCollectionItem)
  private
    FFieldName: string;
    FValue: Variant;
    FTableName: string;
    FOnSQLSubValue: TOnSQLSubValue;
    FPredicate: TWherePredicate;
    FEnabled: boolean;
    procedure SetFieldName(const Value: string);
    procedure SetTableName(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure SetOnSQLSubValue(const Value: TOnSQLSubValue);
    procedure SetPredicate(const Value: TWherePredicate);
    procedure SetEnabled(const Value: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Value : Variant read FValue write SetValue;
  published
    property TableName : string read FTableName write SetTableName;
    property FieldName : string read FFieldName write SetFieldName;
    property Predicate : TWherePredicate read FPredicate write SetPredicate;
    property Enabled : boolean read FEnabled write SetEnabled;
    property OnSQLSubValue : TOnSQLSubValue read FOnSQLSubValue write SetOnSQLSubValue;
  end;

  TSQLSubs = class(TCollection)
  private
    FReportDialog : TCustomReportDialog;
    function GetItem(Index: Integer): TSQLSub;
    procedure SetItem(Index: Integer; const Value: TSQLSub);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ReportDialog: TCustomReportDialog);
    function Add: TSQLSub;
    function AddItem(Item: TSQLSub; Index: Integer): TSQLSub;
    function Insert(Index: Integer): TSQLSub;
    property Items[Index: Integer]: TSQLSub read GetItem write SetItem; default;
  end;

  TfReportSelect = class(TForm)
    panelButtons: TPanel;
    bOK: TButton;
    bCancel: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    comboReportTypes: TComboBox;
    tbAddReport: TToolButton;
    ToolButton2: TToolButton;
    lvReports: TListView;
    Images: TImageList;
    ToolButton1: TToolButton;
    tbCopyReport: TToolButton;
    Actions: TActionList;
    aAddReport: TAction;
    aCopyReport: TAction;
    PopupMenu: TPopupMenu;
    AddReport1: TMenuItem;
    aCopyReport1: TMenuItem;
    aReportProperties: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    N1: TMenuItem;
    ReportProperties1: TMenuItem;
    aPrint: TAction;
    aPreview: TAction;
    ToolButton5: TToolButton;
    aDeleteReport: TAction;
    procedure comboReportTypesChange(Sender: TObject);
    procedure lvReportsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure aAddReportExecute(Sender: TObject);
    procedure aCopyReportExecute(Sender: TObject);
    procedure aReportPropertiesExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aPreviewExecute(Sender: TObject);
    procedure aDeleteReportExecute(Sender: TObject);
  private
    FCR : TCrpe;
    FDB : TADOConnection;
    FReportType : string;
    FReportList : TReportList;
    FReportTypeList : TStringList;
    FTMReports : TTMReports;
    FConnectionStrings : TDelimParser;
    FReporting: boolean;
    function GetSelectedReport: string;
    procedure PerformSubstitutions(SubStrings : TStrings);
    procedure OnClosePreviewWindow(WindowHandle: HWnd; var Cancel: Boolean);
    procedure InitVars(ADB: TADOConnection; AReportType: string);
    procedure SetReporting(const Value: boolean);
    procedure SetPreviewOptions(ACrpe: TCrpe);
    function OrderString: string;
    function GetOrderString: string;
    procedure PrepareReport(RI: TReportInfo);
    procedure CreatePDF(RI : TReportInfo; const DestFile : string);
  public
    constructor CreateEx(AOwner : TComponent; ADB : TADOConnection; AReportType : string);
    constructor CreateMin(AOwner : TComponent; ADB : TADOConnection; AReportType : string);
    destructor Destroy; override;
//    procedure InvokeReport(RI : TReportInfo; Preview : boolean = True);
    procedure InvokeReport(RI : TReportInfo);
    property SelectedReport : string read GetSelectedReport;
    procedure RefreshReportList;
    procedure RefreshReportTypeList;
    property Reporting : boolean read FReporting write SetReporting;
  end;

  TPreviewOption = (rpoAllowDrillDown,rpoCancelBtn,rpoCloseBtn,
    rpoDocumentTips,rpoExportBtn,rpoGroupTree,rpoLaunchBtn,rpoNavigationCtls,
    rpoPrintBtn,rpoPrintSetupBtn,rpoProgressCtls,rpoRefreshBtn,rpoSearchBtn,
    rpoToolbarTips,rpoVisible,rpoZoomCtl);
  TPreviewOptions = set of TPreviewOption;

  TOrderMethod = (omAppend, omReplace);

  TCustomReportDialog = class(TComponent)
  private
    FDatabase: TADOConnection;
    FReportName: TFileName;
    FReportType: string;
    FValidateFile: boolean;
    FAutoLaunch: boolean;
    FSQLSubs: TSQLSubs;
//    FReportButtonBar: TCrpeWindowButtonBar;
    FPreviewOptions : TPreviewOptions;
    FWindowState: TWindowState;
    FSortFields : TStringList;
    FOrderFields: TSQLOrderFields;
    FOrderMethod: TOrderMethod;
    FWindowTitle: string;
    FOnSetParameters: TOnSetParameters;
//    FReportTitle: string;
    procedure SetSQLSubs(const Value: TSQLSubs);
    procedure SetWindowState(const Value: TWindowState);
    procedure SetOrderFields(const Value: TSQLOrderFields);
    procedure SetOrderMethod(const Value: TOrderMethod);
    procedure SetWindowTitle(const Value: string);
    procedure SetOnSetParameters(const Value: TOnSetParameters);
//    function GetReportButtonBar: TCrpeWindowButtonBar;
//    procedure SetReportButtonBar(const Value: TCrpeWindowButtonBar);
//    procedure SetReportTitle(const Value: string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute:boolean; virtual;
    function ExecuteDirect:boolean; overload; virtual;
    function ExecuteDirect(const ReportFile : TFileName):boolean; overload; virtual;
    function CreatePDFDirect(const DestFile: string): boolean;
    function CreatePDF(const ReportFile, DestFile : TFileName):boolean;
    {}
    procedure AddSortField(const FieldName : string);
    procedure HandleOnSetParameters(ParamFields : TCrpeParamFields);
    {}
    property Database : TADOConnection read FDatabase write FDatabase;
    property ReportType : string read FReportType write FReportType;
    property ReportName : TFileName read FReportName write FReportName;
//    property ReportTitle : string read FReportTitle write SetReportTitle;
    property ValidateFile : boolean read FValidateFile write FValidateFile;
    property AutoLaunch : boolean read FAutoLaunch write FAutoLaunch;
    property SQLSubs : TSQLSubs read FSQLSubs write SetSQLSubs;
    property OrderFields : TSQLOrderFields read FOrderFields write SetOrderFields;
    property OrderMethod : TOrderMethod read FOrderMethod write SetOrderMethod;
//    property ReportButtonBar : TCrpeWindowButtonBar read GetReportButtonBar write SetReportButtonBar;
    property PreviewOptions : TPreviewOptions read FPreviewOptions write FPreviewOptions;
    property WindowState : TWindowState read FWindowState write SetWindowState;
    property WindowTitle : string read FWindowTitle write SetWindowTitle;

    { Events }
    property OnSetParameters : TOnSetParameters read FOnSetParameters write SetOnSetParameters;

  end;

  TTMReportDialog = class(TCustomReportDialog)
  published
    property Database;
    property ReportType;
    property ReportName;
//    property ReportTitle;
    property ValidateFile;
    property AutoLaunch;
    property SQLSubs;
    property OrderFields;
//    property OrderMethod;
//    property ReportButtonBar;
    property PreviewOptions;
    property WindowState;
    property WindowTitle;

    { Events }
    property OnSetParameters;
  end;

implementation

uses dialogTMReportInfo;

{$R *.dfm}

{ TCustomReport }

procedure TCustomReportDialog.AddSortField(const FieldName: string);
begin

end;

constructor TCustomReportDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrderMethod := omReplace;
  FSortFields := TStringList.Create;
  FSQLSubs := TSQLSubs.Create(Self);
  FOrderFields := TSQLOrderFields.Create(Self);
  FValidateFile := True;
  FAutoLaunch := False;
  FReportName := '';
  FWindowState := wsNormal;

  FPreviewOptions := [rpoAllowDrillDown,rpoCancelBtn,rpoCloseBtn,
    rpoDocumentTips,rpoExportBtn,rpoGroupTree,rpoLaunchBtn,rpoNavigationCtls,
    rpoPrintBtn,rpoPrintSetupBtn,rpoProgressCtls,rpoRefreshBtn,rpoSearchBtn,
    rpoToolbarTips,rpoVisible,rpoZoomCtl];
end;

destructor TCustomReportDialog.Destroy;
begin
  FOrderFields.Free;
  FSQLSubs.Free;
  FSortFields.Free;
  inherited;
end;

function TCustomReportDialog.Execute: boolean;
begin
//  Result := False;
  FReportName := '';
  if FDatabase = nil then
    raise Exception.Create('No database connection');
  Screen.Cursor := crHourGlass;
  try
    with TfReportSelect.CreateEx(Self,FDatabase,FReportType) do try
      Result := ShowModal = mrOK;
      Result := True;
//      if Result then
//        FReportName := SelectedReport;
    finally
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//procedure TCustomReportDialog.SetReportTitle(const Value: string);
//begin
//  FReportTitle := Value;
//end;

function TCustomReportDialog.ExecuteDirect: boolean;
begin
  Result := ExecuteDirect(ReportName);
end;

function TCustomReportDialog.ExecuteDirect(
  const ReportFile: TFileName): boolean;
var
  RI : TReportInfo;
begin
  with TfReportSelect.CreateMin(Self,FDatabase,FReportType) do try
    RI := TReportInfo.Create(0,
            ReportType,
            ExtractFilePath(ReportFile),
            ExtractFileName(ReportFile),
            '','',True);
    try
      { TODO }
      InvokeReport(RI);
      while Reporting do begin
        Application.ProcessMessages;
        Sleep(250);
      end;
    finally
      RI.Free;
    end;
    FSortFields.Clear;
  finally
    Free;
  end;
end;

procedure TCustomReportDialog.HandleOnSetParameters(
  ParamFields: TCrpeParamFields);
begin
  if Assigned(FOnSetParameters) then
    FOnSetParameters(ParamFields);
end;

function TCustomReportDialog.CreatePDFDirect(const DestFile : string): boolean;
begin
  Result := CreatePDF(ReportName, DestFile);
end;

function TCustomReportDialog.CreatePDF(const ReportFile, DestFile: TFileName): boolean;
var
  RI : TReportInfo;
begin
  with TfReportSelect.CreateMin(Self,FDatabase,FReportType) do try
    RI := TReportInfo.Create(0,
            ReportType,
            ExtractFilePath(ReportFile),
            ExtractFileName(ReportFile),
            '','',True);
    try
      CreatePDF(RI, DestFile);
      while Reporting do begin
        Application.ProcessMessages;
        Sleep(250);
      end;
    finally
      RI.Free;
    end;
    FSortFields.Clear;
  finally
    Free;
  end;
end;

procedure TCustomReportDialog.SetOnSetParameters(const Value: TOnSetParameters);
begin
  FOnSetParameters := Value;
end;

procedure TCustomReportDialog.SetOrderFields(const Value: TSQLOrderFields);
begin
  FOrderFields := Value;
end;

procedure TCustomReportDialog.SetOrderMethod(const Value: TOrderMethod);
begin
  FOrderMethod := Value;
end;

procedure TCustomReportDialog.SetSQLSubs(const Value: TSQLSubs);
begin
  FSQLSubs := Value;
end;

{ TfReportSelect }

constructor TfReportSelect.CreateEx(AOwner: TComponent;
  ADB: TADOConnection; AReportType: string);
begin
  inherited Create(AOwner);
  InitVars(ADB,AReportType);
  RefreshReportList;
  RefreshReportTypeList;
  comboReportTypes.ItemIndex := comboReportTypes.Items.IndexOf(FReportType);
end;

constructor TfReportSelect.CreateMin(AOwner: TComponent;
  ADB: TADOConnection; AReportType: string);
begin
  inherited Create(AOwner);
  InitVars(ADB,AReportType);
end;

destructor TfReportSelect.Destroy;
begin
  FCR.Free;
  FConnectionStrings.Free;
  FReportTypeList.Free;
  FReportList.Free;
  FTMReports.Free;
  inherited;
end;

procedure TfReportSelect.InitVars(ADB: TADOConnection; AReportType: string);
begin
  if ADB = nil then
    raise Exception.Create('Invalid ADO Connection Object');
  FDB := ADB;
  FReportType := AReportType;
  FTMReports := TTMReports.Create(FDB);
  FReportList := TReportList.Create;
  FReportTypeList := TStringList.Create;
  FConnectionStrings := TDelimParser.Create(FDB.ConnectionString,';');
  FCR := TCrpe.Create(nil);
  FReporting := False;
end;

function TfReportSelect.GetSelectedReport: string;
var
  LI : TListItem;
begin
  LI := lvReports.Selected;
  if LI <> nil then begin
    Result := LI.SubItems[0]+LI.SubItems[1];
    if (Owner is TCustomReportDialog) then
    if TCustomReportDialog(Owner).ValidateFile then
      if not FileExists(Result) then
        raise Exception.CreateFmt('%s does not exists',[Result]);
  end;
end;

procedure TfReportSelect.RefreshReportList;
var
  i : integer;
  LI : TListItem;
begin
  if not FDB.Connected then
    FDB.Open;
  FTMReports.ReportsByType(FReportType,FReportList);
  with lvReports do begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for i := 0 to FReportList.Count -1 do begin
        LI := Items.Add;
        LI.Caption := FReportList[i].Description;
        LI.SubItems.Add(FReportList[i].Path);
        LI.SubItems.Add(FReportList[i].Name);
        LI.SubItems.Add(iif_str(FReportList[i].Custom,'Yes','No'));
        LI.ImageIndex := 5;
        LI.Data := FReportList[i];
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TfReportSelect.RefreshReportTypeList;
begin
  if not FDB.Connected then
    FDB.Open;
  FTMReports.ReportTypes(FReportTypeList);
  comboReportTypes.Items.Assign(FReportTypeList);
end;

procedure TfReportSelect.comboReportTypesChange(Sender: TObject);
begin
  FReportType := comboReportTypes.Text;
  Screen.Cursor := crHourglass;
  try
    RefreshReportList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfReportSelect.lvReportsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  aCopyReport.Enabled := (lvReports.Focused and (lvReports.Selected <> nil));
  aReportProperties.Enabled := (lvReports.Focused and (lvReports.Selected <> nil));
  aDeleteReport.Enabled := (lvReports.Focused and (lvReports.Selected <> nil));
  aPrint.Enabled := (lvReports.Focused and (lvReports.Selected <> nil));
end;

procedure TfReportSelect.aAddReportExecute(Sender: TObject);
var
  RI : TReportInfo;
begin
  { Add Report }
  RI := TReportInfo.CreateNew;
  try
    if AddReport(RI,FReportTypeList) then begin
      RI.Custom := True;
      Screen.Cursor := crHourglass;
      try
        FTMReports.AddReport(RI);
        RefreshReportList;
        RefreshReportTypeList;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    RI.Free;
  end;
end;

procedure TfReportSelect.aCopyReportExecute(Sender: TObject);
var
  RI : TReportInfo;
  LI : TListItem;
begin
  { Copy Report }
  LI := lvReports.Selected;
  if LI <> nil then begin
    RI := TReportInfo(LI.Data);
    Screen.Cursor := crHourglass;
    try
      RI.Description := 'Copy of '+RI.Description;
      RI.Custom := True;
      FTMReports.AddReport(RI);
      RefreshReportList;
      RefreshReportTypeList;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfReportSelect.aDeleteReportExecute(Sender: TObject);
var
  RI : TReportInfo;
  LI : TListItem;
begin
  { Delete Report }
  LI := lvReports.Selected;
  if LI <> nil then begin
    RI := TReportInfo(LI.Data);
    if not RI.Custom then
      raise Exception.Create('Cannot delete a report that is not custom!');
    if ConfirmYN('Are you sure you wish to delete this report?') then begin
      Screen.Cursor := crHourglass;
      try
        FTMReports.DeleteReport(RI);
        RefreshReportList;
        RefreshReportTypeList;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfReportSelect.aReportPropertiesExecute(Sender: TObject);
var
  RI : TReportInfo;
  LI : TListItem;
begin
  { Edit Report }
  LI := lvReports.Selected;
  if LI <> nil then begin
    RI := TReportInfo(LI.Data);
    if EditReport(RI,FReportTypeList) then begin
      Screen.Cursor := crHourglass;
      try
        FTMReports.UpdateReport(RI);
        RefreshReportList;
        RefreshReportTypeList;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TfReportSelect.aPrintExecute(Sender: TObject);
var
  LI : TListItem;
  RI : TReportInfo;
begin
  LI := lvReports.Selected;
  if LI <> nil then begin
    RI := TReportInfo(LI.Data);
    { TODO }
//    InvokeReport(RI,False);
    InvokeReport(RI);
  end
end;

procedure TfReportSelect.aPreviewExecute(Sender: TObject);
var
  LI : TListItem;
  RI : TReportInfo;
begin
  LI := lvReports.Selected;
  if LI <> nil then begin
    RI := TReportInfo(LI.Data);
    { TODO }
    InvokeReport(RI);
  end;
end;

procedure TfReportSelect.SetPreviewOptions(ACrpe : TCrpe);
begin
  if Owner is TCustomReportDialog then
    with Owner as TCustomReportDialog do begin
      ACrpe.WindowButtonBar.AllowDrillDown := rpoAllowDrillDown in PreviewOptions;
      ACrpe.WindowButtonBar.CancelBtn := rpoCancelBtn in PreviewOptions;
      ACrpe.WindowButtonBar.CloseBtn := rpoCloseBtn in PreviewOptions;
      ACrpe.WindowButtonBar.DocumentTips := rpoDocumentTips in PreviewOptions;
      ACrpe.WindowButtonBar.ExportBtn := rpoExportBtn in PreviewOptions;
      ACrpe.WindowButtonBar.GroupTree := rpoGroupTree in PreviewOptions;
      ACrpe.WindowButtonBar.LaunchBtn := rpoLaunchBtn in PreviewOptions;
      ACrpe.WindowButtonBar.NavigationCtls := rpoNavigationCtls in PreviewOptions;
      ACrpe.WindowButtonBar.PrintBtn := rpoPrintBtn in PreviewOptions;
      ACrpe.WindowButtonBar.PrintSetupBtn := rpoPrintSetupBtn in PreviewOptions;
      ACrpe.WindowButtonBar.ProgressCtls := rpoProgressCtls in PreviewOptions;
      ACrpe.WindowButtonBar.RefreshBtn := rpoRefreshBtn in PreviewOptions;
      ACrpe.WindowButtonBar.SearchBtn := rpoSearchBtn in PreviewOptions;
      ACrpe.WindowButtonBar.ToolbarTips := rpoToolbarTips in PreviewOptions;
      ACrpe.WindowButtonBar.Visible := rpoVisible in PreviewOptions;
      ACrpe.WindowButtonBar.ZoomCtl := rpoZoomCtl in PreviewOptions;
    end;
end;

function TfReportSelect.OrderString:string;
begin
  {}
end;

//procedure TfReportSelect.InvokeReport(RI : TReportInfo; Preview: boolean = True);
procedure TfReportSelect.InvokeReport(RI : TReportInfo);
begin
  Screen.Cursor := crHourGlass;
  try
    PrepareReport(RI);
    if FCR.Connect.Test then begin
      Reporting := True;
      try
        FCR.Execute;
      except
        Reporting := False;
        raise;
      end
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfReportSelect.CreatePDF(RI : TReportInfo; const DestFile : string);
begin
  Screen.Cursor := crHourGlass;
  try
    PrepareReport(RI);
    FCR.Connect.Propagate := True;
    if FCR.Connect.Test then begin
      with FCR.ExportOptions do begin
        Destination := toFile;
        FileType := AdobeAcrobatPDF;
        FileName := DestFile;
      end;
      Reporting := True;
      try
        try
          FCR.Export;
        except
          raise;
        end
      finally
        Reporting := False;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfReportSelect.PrepareReport(RI : TReportInfo);
var
  ReportSQL, WhereSQL, NewSQL : TStringList;
  NewSQLStr : string;
  i, j, k, l : integer;
  FoundWhere, DidSub : boolean;
  s : string;
  OrderString : string;
begin
  if RI = nil then
    raise Exception.Create('Invalid Report Infomation');
  FCR.WindowEvents := True;
  SetPreviewOptions(FCR);
  FCR.WindowZoom.Preview := pwWholePage;
//    FCR.WindowButtonBar := TCustomReportDialog(Owner).ReportButtonBar;
//    FCR.WindowButtonBar.ActivateAll;
  FCR.wOnCloseWindow := OnClosePreviewWindow;
  FCR.ReportName := RI.ReportName;
  FCR.Connect.Clear;
  FCR.Connect.Propagate := True;
  FCR.Connect.ServerName := FConnectionStrings.Elements['Data Source'];
  
  if Length(Trim(FConnectionStrings.Elements['UID'])) = 0 then
    FCR.Connect.UserID := FConnectionStrings.Elements['User ID']
  else
    FCR.Connect.UserID := FConnectionStrings.Elements['UID'];

  if Length(Trim(FConnectionStrings.Elements['PWD'])) = 0 then
    FCR.Connect.Password := FConnectionStrings.Elements['Password']
  else
    FCR.Connect.Password := FConnectionStrings.Elements['PWD'];
    
//  FCR.Connect.DatabaseName := '<CRWDC>Database=' + FConnectionStrings.Elements['Data Source'];
  FCR.Output := toWindow;
  if (Owner is TCustomReportDialog) then begin
    FCR.WindowState := TCustomReportDialog(Owner).WindowState;
    if Length(TCustomReportDialog(Owner).WindowTitle) = 0 then
      TCustomReportDialog(Owner).WindowTitle := DefaultWindowTitle;
    FCR.WindowStyle.Title := TCustomReportDialog(Owner).WindowTitle;
    FCR.SummaryInfo.Title := TCustomReportDialog(Owner).WindowTitle;

    { Allow the application to handle parameters }
    TCustomReportDialog(Owner).HandleOnSetParameters(FCR.ParamFields);
  end;


  WhereSQL := TStringList.Create;
  try
    PerformSubstitutions(WhereSQL);
    OrderString := GetOrderString;
    if ((WhereSQL.Count > 0) or (Length(OrderString) > 0))then begin
      ReportSQL := TStringList.Create;
      NewSQL := TStringList.Create;
      try
        FCR.SQL.Retrieve;
//        ReportSQL.Assign(FCR.SQL.Query);
//        FCR.SQL.Clear;
        DidSub := False;
        FoundWhere := False; // fix
//        for i := 0 to ReportSQL.Count -1 do begin
        for i := 0 to FCR.SQL.Query.Count -1 do begin
          { There may be no where clause }
//          s := Uppercase(Trim(ReportSQL[i]));
          s := Uppercase(Trim(FCR.SQL.Query[i]));
          { We expect the where clause to be at the beginning }
          if Pos('WHERE',s) = 1 then begin
            if not FoundWhere then
              FoundWhere := True;
          end;
          if Pos('ORDER BY',s) = 1 then begin
            for j := 0 to WhereSQL.Count -1 do begin
              if not FoundWhere then begin
//                FCR.SQL.Query.Add(Format(' WHERE %s ',[WhereSQL[j]]));
                FCR.SQL.Query.Insert(i, Format(' WHERE %s ',[WhereSQL[j]]));
                FoundWhere := True;
              end
              else
//                FCR.SQL.Query.Add(Format(' AND %s ',[WhereSQL[j]]));
                FCR.SQL.Query.Insert(i, Format(' AND %s ',[WhereSQL[j]]));
            end;
            if Length(OrderString) > 0 then begin
              if (Owner is TCustomReportDialog) then begin
                if (Owner as TCustomReportDialog).OrderMethod = omReplace then begin
                  FCR.SQL.Query.Add(' ORDER BY '+OrderString);
                  Break;
                end;
              end;
            end;
            DidSub := True;
          end;
//          if Length(Trim(s)) > 0 then
//            FCR.SQL.Query.Add(Trim(s));
        end;
        if not DidSub then begin
          for k := 0 to WhereSQL.Count -1 do begin
            if not FoundWhere then begin
              FCR.SQL.Query.Add(Format(' WHERE %s ',[WhereSQL[k]]));
              FoundWhere := True;
            end
            else
              FCR.SQL.Query.Add(Format(' AND %s ',[WhereSQL[k]]));
          end;
          if Length(OrderString) > 0 then
            FCR.SQL.Query.Add(' ORDER BY '+OrderString);
          DidSub := True;
        end;
//        FCR.SQL.Clear;
//        FCR.SQL.Query := NewSQL;
//        FCR.SQL.Query.Add(NewSQLStr);
      finally
        NewSQL.Free;
        ReportSQL.Free;
      end;
    end;
    //WCCTraceStrings(FCR.SQL.Query);
  finally
    WhereSQL.Free;
  end;
end;

function TfReportSelect.GetOrderString:string;
var
  i : integer;
  RD : TCustomReportDialog;
begin
  Result := '';
  if Owner is TCustomReportDialog then begin
    RD := Owner as TCustomReportDialog;
    for i := 0 to RD.OrderFields.Count -1 do begin
      if i > 0 then Result := Result + ', ';
      Result := RD.OrderFields[i].FFieldName +
                  iif_str(RD.OrderFields[i].Direction = dAscending,' ASC', ' DESC');
    end;
  end;
end;

procedure TfReportSelect.PerformSubstitutions(SubStrings : TStrings);
var
  i : integer;
  RD : TCustomReportDialog;
  V : Variant;
  s : string;
begin
  if SubStrings = nil then
    raise Exception.Create('Invalid Strings');
  SubStrings.Clear;
  if Owner is TCustomReportDialog then begin
    RD := Owner as TCustomReportDialog;
    for i := 0 to RD.SQLSubs.Count -1 do begin
      V := NULL;
      if not RD.SQLSubs.Items[i].Enabled then
        Continue;
      V := RD.SQLSubs.Items[i].Value;
      if Assigned(RD.SQLSubs.Items[i].OnSQLSubValue) then
        RD.SQLSubs.Items[i].OnSQLSubValue(V);
      if not (VarIsEmpty(V) or VarIsNull(V)) then begin
        case VarType(V) of
          varDate : s := format(' %s."%s" %s ''%s'' ',[RD.SQLSubs.Items[i].TableName,
                                                         RD.SQLSubs.Items[i].FieldName,
                                                         WherePredicates[RD.SQLSubs.Items[i].Predicate],
                                                         FormatDateTime('yyyy-mm-dd-hh.nn.ss.000000',V)]);
          varBoolean : s := format(' %s."%s" %s ''%s'' ',[RD.SQLSubs.Items[i].TableName,
                                                            RD.SQLSubs.Items[i].FieldName,
                                                            WherePredicates[RD.SQLSubs.Items[i].Predicate],
                                                            iif_str(V,'True','False')]);
          varOleStr,
          varStrArg,
          varString : s := format(' %s."%s" %s ''%s'' ',[RD.SQLSubs.Items[i].TableName,
                                                           RD.SQLSubs.Items[i].FieldName,
                                                           WherePredicates[RD.SQLSubs.Items[i].Predicate],
                                                           V]);
          varSmallint,
          varInteger,
          varSingle,
          varDouble,
          varCurrency,
          varInt64,
          varShortInt,
          varByte,
          varWord,
          varLongWord : s := format(' %s."%s" %s %s ',[RD.SQLSubs.Items[i].TableName,
                                                         RD.SQLSubs.Items[i].FieldName,
                                                         WherePredicates[RD.SQLSubs.Items[i].Predicate],
                                                         V]);
          varEmpty,
          varNull,
          varAny,
          varDispatch,
          varError,
          varUnknown,
          varVariant : Continue;
        else
          Continue;
        end;
        SubStrings.Add(s);
      end;
    end;
  end;
end;

procedure TfReportSelect.OnClosePreviewWindow(WindowHandle: HWnd;
  var Cancel: Boolean);
begin
  Reporting := False;
  if Visible then Close;
end;

procedure TfReportSelect.SetReporting(const Value: boolean);
begin
  FReporting := Value;
end;

{ TSQLSub }

procedure TSQLSub.Assign(Source: TPersistent);
begin
  if Source is TSQLSub then
  begin
    TableName := TSQLSub(Source).TableName;
    FieldName := TSQLSub(Source).FieldName;
    Value := TSQLSub(Source).Value;
  end
  else inherited Assign(Source);
end;

constructor TSQLSub.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
end;

function TSQLSub.GetDisplayName: string;
begin
  Result := '';
  if (Length(FTableName) + Length(FFieldName)) > 0 then
    Result := Format('%s.%s',[FTableName,FFieldName])
  else
    Result := inherited GetDisplayName;
end;

procedure TSQLSub.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TSQLSub.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TSQLSub.SetOnSQLSubValue(const Value: TOnSQLSubValue);
begin
  FOnSQLSubValue := Value;
end;

procedure TSQLSub.SetPredicate(const Value: TWherePredicate);
begin
  FPredicate := Value;
end;

procedure TSQLSub.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

procedure TSQLSub.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TSQLSubs }

function TSQLSubs.Add: TSQLSub;
begin
  Result := TSQLSub(inherited Add);
end;

function TSQLSubs.AddItem(Item: TSQLSub; Index: Integer): TSQLSub;
begin
  if Item = nil then
    Result := TSQLSub.Create(Self)
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

constructor TSQLSubs.Create(ReportDialog: TCustomReportDialog);
begin
  inherited Create(TSQLSub);
  FReportDialog := ReportDialog;
end;

function TSQLSubs.GetItem(Index: Integer): TSQLSub;
begin
  Result := TSQLSub(inherited GetItem(Index));
end;

function TSQLSubs.GetOwner: TPersistent;
begin
  Result := FReportDialog;
end;

function TSQLSubs.Insert(Index: Integer): TSQLSub;
begin
  Result := AddItem(nil, Index);
end;

procedure TSQLSubs.SetItem(Index: Integer; const Value: TSQLSub);
begin
  inherited SetItem(Index, Value);
end;

procedure TSQLSubs.Update(Item: TCollectionItem);
begin
  inherited;
end;

procedure TCustomReportDialog.SetWindowState(const Value: TWindowState);
begin
  FWindowState := Value;
end;

{ TSQLOrderField }

procedure TSQLOrderField.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TSQLOrderField.SetDirection(const Value: TDirection);
begin
  FDirection := Value;
end;

procedure TSQLOrderField.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TSQLOrderField.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TSQLOrderField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
end;

procedure TSQLOrderField.Assign(Source: TPersistent);
begin
  if Source is TSQLOrderField then
  begin
    FieldName := TSQLOrderField(Source).FieldName;
    Direction  := TSQLOrderField(Source).Direction;
  end
  else inherited Assign(Source);
end;

function TSQLOrderField.GetDisplayName: string;
begin
  Result := '';
  if Length(FFieldName) > 0 then
    Result := FFieldName + iif_str(FDirection=dAscending,' Asc',' Desc')
  else
    Result := inherited GetDisplayName;
end;

{ TSQLOrderFields }

function TSQLOrderFields.GetItem(Index: Integer): TSQLOrderField;
begin
  Result := TSQLOrderField(inherited GetItem(Index));
end;

procedure TSQLOrderFields.SetItem(Index: Integer; const Value: TSQLOrderField);
begin
  inherited SetItem(Index, Value);
end;

constructor TSQLOrderFields.Create(ReportDialog: TCustomReportDialog);
begin
  inherited Create(TSQLOrderField);
  FReportDialog := ReportDialog;
end;

function TSQLOrderFields.Add: TSQLOrderField;
begin
  Result := TSQLOrderField(inherited Add);
end;

function TSQLOrderFields.AddItem(Item: TSQLOrderField;
  Index: Integer): TSQLOrderField;
begin
  if Item = nil then
    Result := TSQLOrderField.Create(Self)
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

procedure TSQLOrderFields.Update(Item: TCollectionItem);
begin
  inherited;
end;

function TSQLOrderFields.Insert(Index: Integer): TSQLOrderField;
begin
  Result := AddItem(nil, Index);
end;

function TSQLOrderFields.GetOwner: TPersistent;
begin
  Result := FReportDialog;
end;

procedure TCustomReportDialog.SetWindowTitle(const Value: string);
begin
  FWindowTitle := Value;
end;

end.
