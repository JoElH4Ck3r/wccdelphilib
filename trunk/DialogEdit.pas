unit DialogEdit;

{$R WCCImages.res}

interface

uses Classes, Windows, Messages, JvToolEdit, Controls, DBSearch, ImgList,
  Graphics, SysUtils;

type
  TExecOpenSearchDialogEvent = procedure(Sender: TObject; var SearchValue: string;
    var Action: Boolean) of object;

  TCustomDialogEdit = class(TJvCustomComboEdit)
  private
    FSearchField: string;
    FSearchDialog: TCustomDBSearchDialog;
    FOnAfterDialog: TExecOpenSearchDialogEvent;
    FOnBeforeDialog: TExecOpenSearchDialogEvent;
    FSearchValue : string;
    FDisplayField: string;
    procedure SetSearchDialog(const Value: TCustomDBSearchDialog);
    procedure SetSearchField(const Value: string);
    function GetSearchValue: string;
    procedure SetSearchValue(const Value: string);
    function GetSearchDialog: TCustomDBSearchDialog;
    procedure SetDisplayField(const Value: string);
    {}
  protected
    procedure DoAfterDialog(var ASearchValue: string; var Action: Boolean); dynamic;
    procedure DoBeforeDialog(var ASearchValue: string; var Action: Boolean); dynamic;
    procedure PopupDropDown(DisableEdit: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    class function DefaultImageIndex: TImageIndex; override;
    property SearchValue : string read GetSearchValue write SetSearchValue;
    { Publishable }
    property SearchDialog : TCustomDBSearchDialog read GetSearchDialog write SetSearchDialog;
    property SearchField : string read FSearchField write SetSearchField;
    property DisplayField : string read FDisplayField write SetDisplayField;
  published
    property OnBeforeDialog: TExecOpenSearchDialogEvent read FOnBeforeDialog
      write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenSearchDialogEvent read FOnAfterDialog
      write FOnAfterDialog;
  end;

  TDialogEdit = class(TCustomDialogEdit)
  published
    { From TCustomDialogEdit}
    property SearchDialog;
    property SearchField;
    property DisplayField;
    property AlwaysEnableButton;
    { From TJvCustomComboEdit }
    property Action;
    property Align;
    property AutoSize;
    property Flat;
    property ParentCtl3D;
    property AutoSelect;
    property ButtonHint;
    property ButtonFlat;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property DirectInput;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex;
    property Images;
    property ImageKind;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
  end;

implementation

uses Dialogs, Forms;

const
  sSearcg12BMP = 'SEARCH_12';
  sSearcg14BMP = 'SEARCH_14';
  sSearcg16BMP = 'SEARCH_16';
  sSearcg24BMP = 'SEARCH_24';

var
  GSearchImageIndex: TImageIndex = -1;

{ TCustomDialogEdit }

constructor TCustomDialogEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlState := ControlState + [csCreating];
  try
    ImageKind := ikDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TCustomDialogEdit.Destroy;
begin
  {}
  inherited;
end;

procedure TCustomDialogEdit.SetSearchField(const Value: string);
begin
  FSearchField := Value;
end;

procedure TCustomDialogEdit.SetSearchDialog(const Value: TCustomDBSearchDialog);
begin
  FSearchDialog := Value;
end;

function TCustomDialogEdit.GetSearchDialog: TCustomDBSearchDialog;
begin
  Result := FSearchDialog;
end;

class function TCustomDialogEdit.DefaultImageIndex: TImageIndex;
var
  Bmp: TBitmap;
begin
  if GSearchImageIndex < 0 then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HInstance, sSearcg14BMP);
      GSearchImageIndex := DefaultImages.AddMasked(Bmp, clFuchsia);
    finally
      Bmp.Free;
    end;
  end;
  Result := GSearchImageIndex;
end;

procedure TCustomDialogEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSearchDialog) then
    SearchDialog := nil;
end;

procedure TCustomDialogEdit.PopupDropDown(DisableEdit: Boolean);
var
  Temp : string;
  Action : boolean;
begin
  inherited;
  Screen.Cursor := crHourglass;
  try

    if SearchDialog = nil then
      raise Exception.Create('No search dialog specified');

    Temp := Text; //SearchValue;

    Action := True;
    { invoke the before event }
    DoBeforeDialog(Temp, Action);

    SearchDialog.Value := Temp;

    Action := SearchDialog.Execute;
    if Action then
      Temp := SearchDialog.Value;

    DoAfterDialog(Temp, Action);

    SearchValue := Temp
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCustomDialogEdit.DoBeforeDialog(var ASearchValue: string;
  var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then
    FOnBeforeDialog(Self, ASearchValue, Action);
end;

procedure TCustomDialogEdit.DoAfterDialog(var ASearchValue: string;
  var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then
    FOnAfterDialog(Self, ASearchValue, Action);
end;

function TCustomDialogEdit.GetSearchValue: string;
begin
  Result := FSearchValue;
end;

procedure TCustomDialogEdit.SetSearchValue(const Value: string);
begin
  if FSearchValue <> Value then begin
    FSearchValue := Value;
    Text := Value;
  end;
end;

procedure TCustomDialogEdit.SetDisplayField(const Value: string);
begin
  FDisplayField := Value;
end;

end.
