unit AppClassesBase;

interface

uses Classes;

type
  TBaseDBObject = class(TPersistent)
  private
    FUpdateCount : integer;
    function GetUpdating: boolean;
  protected
  public
    constructor Create; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    {}
    property Updating : boolean read GetUpdating;
  end;


implementation

{ TBaseDBObject }

constructor TBaseDBObject.Create;
begin
  inherited Create;
  FUpdateCount := 0;
end;

procedure TBaseDBObject.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

procedure TBaseDBObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TBaseDBObject.GetUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

end.
