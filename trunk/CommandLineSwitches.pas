unit CommandLineSwitches;

interface

uses Classes;

type

  TCommandLineSwitches = class(TPersistent)
  private
    FUsername: string;
    FPassword: string;
    FDatabase: string;
    FSchema  : string;
    FPath: string;
  public
    constructor Create;
  published
    property Username : string read FUsername write FUsername;
    property Password : string read FPassword write FPassword;
    property Database : string read FDatabase write FDatabase;
    property Schema   : string read FSchema   write FSchema;
    property Path     : string read FPath     write FPath;
  end;

implementation

{ TCommandLineSwitches }

constructor TCommandLineSwitches.Create;
begin
  inherited Create;
  FUsername := '-u';
  FPassword := '-p';
  FDatabase := '-d';
  FSchema   := '-s';
  FPath     := '-f';
end;

end.
