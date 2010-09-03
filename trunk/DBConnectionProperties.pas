unit DBConnectionProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ADODB;

type
  TFConnectionProperties = class(TForm)
    Panel1: TPanel;
    bClose: TButton;
    mProperties: TMemo;
  private
    FDB : TADOConnection;
  public
    constructor Create(AOwner : TComponent; ADB : TADOConnection);
  end;

var
  FConnectionProperties: TFConnectionProperties;

implementation

{$R *.dfm}

{ TFConnectionProperties }

constructor TFConnectionProperties.Create(AOwner: TComponent;
  ADB: TADOConnection);
var
  i : integer;
begin
  inherited Create(AOwner);

  FDB := ADB;

  for i := 0 to FDB.Properties.Count - 1 do
    mProperties.Lines.Add(
      Format('%s = %s',[
        FDB.Properties.Item[i].Name,
        VarToStr(FDB.Properties.Item[i].Value)
        ])
      );

end;

end.
