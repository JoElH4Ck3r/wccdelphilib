unit CustomWCCConnection;

interface

uses Classes, SysUtils, ADODB, ADOINT;

type
  TDatabaseServer = (dsDB2, dsMSSQL);
  TDatabaseServers = set of TDatabaseServer;

  TCustomWCCConnection = class(TADOConnection)
  private
    FSchemaName: string;
    FPathName: string;
    FSetSchema: boolean;
    FSetPath: boolean;
    FUserName: string;
    function GetServerType: TDatabaseServer;
  protected
    procedure DoConnect; override;
  public
    constructor Create(AOwner : TComponent); override;
    property DatabaseServerType : TDatabaseServer read GetServerType;
    property UserName : string read FUserName write FUserName;
  published
    property SetSchema : boolean read FSetSchema write FSetSchema;
    property SchemaName : string read FSchemaName write FSchemaName;
    property SetPath : boolean read FSetPath write FSetPath;
    property PathName : string read FPathName write FPathName;
  end;

implementation

uses
  Variants
//  , RREDUtil
  , wccutil
  ;

const
  DefaultSchema = 'LYNX';
  DefaultPath   = 'LYNX';

{ TCustomWCCConnection }

constructor TCustomWCCConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { This is rather TruckMate specific }                         

  FSetSchema  := True;
  FSchemaName := DefaultSchema;
  FSetPath    := True;
  FPathName   := DefaultPath;
end;

procedure TCustomWCCConnection.DoConnect;
begin
  inherited;
  //InfoMsg('TCustomWCCConnection.DoConnect BEGIN');
  if Connected and (SetSchema or SetPath) then begin
    with TADOQuery.Create(nil) do try
      Connection := Self;
      if Length(FSchemaName) = 0 then
        raise Exception.Create('No schema specified');
      if (SetSchema and (Length(FSchemaName) > 0)) then begin
        SQL.Clear;
        SQL.Add(Format('SET SCHEMA %s',[FSchemaName]));
        ExecSQL;
      end;
      if (SetPath and (Length(FPathName) > 0)) then begin
        SQL.Clear;
        SQL.Add(Format('SET PATH SYSFUN,%s',[FPathName]));
        ExecSQL;
      end;
    finally
      Free;
    end;
  end;
  //InfoMsg('TCustomWCCConnection.DoConnect END');
end;

function TCustomWCCConnection.GetServerType: TDatabaseServer;
const
  C_DriverName = 'Driver Name';
  C_ProviderName = 'Provider Name';
  {}
  C_DB2_DriverName = 'DB2CLI.DLL';
  C_MSNative_DriverName = 'SQLNCLI.DLL';
  C_MSSQL_DriverName = 'SQLSRV32.DLL';
  C_MSSQL_OLEDB_Provider = 'sqloledb.dll';
var
  i : integer;
begin
  {TODO: Setup a dsUnknown member for better error discovery }
  Result := dsDB2;

  //InfoMsg(Format('Entering GetServerType (Properties.Count = %d', [Properties.Count]));


  { This needs to be better, for now it will suffice }
  for i := 0 to Properties.Count - 1 do begin

//    DebugTraceFmt('Looking for "%s" or "%s" "%s" = "%s"', [
//      C_DriverName,
//      C_ProviderName,
//      Properties[i].Name,
//      VarToStr(Properties[i].Value)]);


    if CompareText(C_DriverName, Properties[i].Name) = 0 then begin

      if CompareText(VarToStr(Properties[i].Value), C_DB2_DriverName) = 0 then
        Result := dsDB2
      else if CompareText(VarToStr(Properties[i].Value), C_MSSQL_DriverName) = 0 then
        Result := dsMSSQL
      else if CompareText(VarToStr(Properties[i].Value), C_MSNative_DriverName) = 0 then
        Result := dsMSSQL
      else
        raise Exception.Create('TWCCConnection.GetServerType Unsupported driver');
      Break;
//    end
//    { We also look for "Provider Name" in the case of OLEDB driver (RRED control panel ) }
//    else if CompareText(C_ProviderName, Properties[i].Name) = 0 then begin
//      if CompareText(VarToStr(Properties[i].Value), C_MSSQL_OLEDB_Provider) = 0 then
//        Result := dsMSSQL;
//
//      Break;
    end;

  end;
//  DebugTraceFmt('TCustomWCCConnection.GetServerType Result = %s',[iif_str(Result=dsDB2,'DB2','MSSQL')]);
end;

end.
