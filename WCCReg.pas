unit WCCReg;

interface

{$I WCC.inc}

procedure Register;

implementation

uses
    Windows
  , Classes
  , DesignEditors
  , DesignIntf
  , ToolsAPI
  , SysUtils
  , LoginDlg
  , dialogTMReportSelect
  , DBSearch
  , DBConnection
  , DialogEdit
  , WCCPropertyEditors
  , DBSearchDlg
  ;

procedure Register;
begin
  RegisterComponents(
    'West Coast Code',
    [ TWCCLoginDlg
    , TTMReportDialog
    , TDBSearchDialog
    , TDriverSearchDialog
    , TTruckSearchDialog
    , TTrailerSearchDialog
    , TCargoSearchDialog
    , TVendorSearchDialog
    , TClientSearchDialog
    , TZoneSearchDialog
    , TWCCConnection
    , TDialogEdit
    , TWCCSearchDialog
    ]);

  RegisterPropertyEditor(TypeInfo(string),TDBSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TDriverSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TTruckSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TTrailerSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TCargoSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TVendorSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TClientSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TZoneSearchDialog,'ReturnField',TDBSearchKeyFieldProperty);
end;

end.
