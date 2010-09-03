unit WCCPropertyEditors;

interface

uses Classes, DesignEditors, DesignIntf, DBSearch;

type
  TDBSearchKeyFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDBSearchDlgKeyFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses Dialogs;

{ TDBSearchKeyFieldProperty }

function TDBSearchKeyFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TDBSearchKeyFieldProperty.GetValues(Proc: TGetStrProc);
var
  SearchDlg : TCustomDBSearchDialog;
  i : integer;
begin
  SearchDlg := GetComponent(0) as TCustomDBSearchDialog;
  for i := 0 to SearchDlg.SearchColumns.Count - 1 do
    Proc(SearchDlg.SearchColumns.Items[i].FieldName);
end;

{ TDBSearchDlgKeyFieldProperty }

function TDBSearchDlgKeyFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TDBSearchDlgKeyFieldProperty.GetValues(Proc: TGetStrProc);
var
  SearchDlg : TCustomDBSearchDialog;
  i : integer;
begin
  SearchDlg := GetComponent(0) as TCustomDBSearchDialog;
  for i := 0 to SearchDlg.SearchColumns.Count - 1 do
    Proc(SearchDlg.SearchColumns.Items[i].FieldName);
end;

end.
