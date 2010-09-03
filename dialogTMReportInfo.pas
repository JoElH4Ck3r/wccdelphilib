unit dialogTMReportInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Mask, { JvToolEdit,} classTMReports,
  JvExMask, JvToolEdit;

type
  TfReportInfo = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    bOK: TButton;
    bCancel: TButton;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    editDescription: TEdit;
    editReportFileName_old: TEdit;
    cbReportType: TComboBox;
    Label3: TLabel;
    rgCustom: TRadioGroup;
    Label4: TLabel;
    labelID: TLabel;
    editReportFileName: TJvFilenameEdit;
  private
  public
    constructor CreateEx(AOwner : TComponent; ReportTypeList : TStrings);
    procedure GetReportInfo(ReportInfo : TReportInfo);
  end;

function AddReport(ReportInfo : TReportInfo; ReportTypes : TStrings):boolean;
function EditReport(ReportInfo : TReportInfo; ReportTypes : TStrings):boolean;

implementation

uses WCCUtil;

{$R *.dfm}

function AddReport(ReportInfo : TReportInfo; ReportTypes : TStrings):boolean;
begin
  if ReportInfo = nil then
    raise Exception.Create('Uninitialized report object');
  with TfReportInfo.CreateEx(nil,ReportTypes) do try
    Result := ShowModal = mrOK;
    if Result then
      GetReportInfo(ReportInfo);
  finally
    Free;
  end;;
end;

function EditReport(ReportInfo : TReportInfo; ReportTypes : TStrings):boolean;
begin
  if ReportInfo = nil then
    raise Exception.Create('Uninitialized report object');
  with TfReportInfo.CreateEx(nil,ReportTypes) do try
    labelID.Caption := IntToStr(ReportInfo.ID);
    cbReportType.Text := ReportInfo._Type;
    editReportFileName.FileName := ReportInfo.Path + ReportInfo.Name;
    rgCustom.ItemIndex := iif_int(ReportInfo.Custom,0,1);
    editDescription.Text := ReportInfo.Description;
    Result := ShowModal = mrOK;
    if Result then
      GetReportInfo(ReportInfo);
  finally
    Free;
  end;
end;

{ TfReportInfo }

constructor TfReportInfo.CreateEx(AOwner: TComponent;
  ReportTypeList: TStrings);
begin
  inherited Create(AOwner);
  cbReportType.Items.Assign(ReportTypeList);
end;

procedure TfReportInfo.GetReportInfo(ReportInfo: TReportInfo);
begin
  if ReportInfo = nil then
    raise Exception.Create('Uninitialized report object');
  with ReportInfo do begin
    //ID := 0;
    _Type := cbReportType.Text;
    Path := ExtractFilePath(editReportFileName.FileName);
    Name := ExtractFileName(editReportFileName.FileName);
    Custom := rgCustom.ItemIndex = 0;
    Description := editDescription.Text;
  end;
end;

end.
