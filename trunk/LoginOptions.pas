unit LoginOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TLoginOptionsData = class(TPersistent)
  private
    FSchema: string;
    FPath: string;
  public
    procedure Assign(Source : TPersistent); override;
    property Schema : string read FSchema write FSchema;
    property Path : string read FPath write FPath;
  end;

  TFLoginOptions = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    tsGeneral: TTabSheet;
    bCancel: TButton;
    bOK: TButton;
    eSchema: TLabeledEdit;
    Label1: TLabel;
    ePath: TLabeledEdit;
    Label2: TLabel;
    procedure eSchemaChange(Sender: TObject);
    procedure ePathChange(Sender: TObject);
  private
    FLoginOptionsData : TLoginOptionsData;
  public
    constructor Create(AOwner : TComponent; ALoginOptionsData : TLoginOptionsData); reintroduce;
    destructor Destroy; override;
    property LoginOptionsData : TLoginOptionsData read FLoginOptionsData;
  end;

//var
//  FLoginOptions: TFLoginOptions;

implementation

{$R *.dfm}

{ TLoginOptions }

procedure TLoginOptionsData.Assign(Source: TPersistent);
begin
  if Source is TLoginOptionsData then begin
    FSchema := (Source as TLoginOptionsData).Schema;
    FPath := (Source as TLoginOptionsData).Path;
  end
  else
    inherited;
end;

{ TFLoginOptions }

constructor TFLoginOptions.Create(AOwner: TComponent;
  ALoginOptionsData: TLoginOptionsData);
begin
  inherited Create(AOwner);
  FLoginOptionsData := TLoginOptionsData.Create;
  if ALoginOptionsData <> nil then
    FLoginOptionsData.Assign(ALoginOptionsData);

  eSchema.Text := FLoginOptionsData.Schema;
  ePath.Text := FLoginOptionsData.Path;

  eSchema.OnChange := eSchemaChange;
  ePath.OnChange := ePathChange;
end;

destructor TFLoginOptions.Destroy;
begin
  FLoginOptionsData.Free;
  inherited;
end;

procedure TFLoginOptions.ePathChange(Sender: TObject);
begin
  FLoginOptionsData.Path := ePath.Text;
end;

procedure TFLoginOptions.eSchemaChange(Sender: TObject);
begin
  FLoginOptionsData.Schema := eSchema.Text;
end;

end.
