unit ODBC;

interface

uses Classes, Sysutils;

const
  C_DB2_7_2_DriverName = 'IBM DB2 ODBC DRIVER';
  C_INTERBASE_56_DriverName = 'INTERSOLV InterBase ODBC Driver (*.gdb)';
  C_SQL_Server = 'SQL Server';
  C_SQL_Native_Client = 'SQL Native Client';

procedure GetDSNList(sList : TStrings); overload;
procedure GetDSNList(sList : TStrings; const DSNType : string); overload;
//procedure GetDSNList(sList : TStrings; DSNTypeList : TStrings); overload;
procedure GetDSNList(sList : TStrings; DSNTypeList : array of String); overload;

implementation



type
  SQLSMALLINT  = SmallInt;
  SQLUSMALLINT = Word;
  SQLCHAR      = Char;
  PSQLCHAR     = PChar;
  SQLRETURN    = SQLSMALLINT;
  SQLHANDLE    = Pointer;
  SQLHENV      = SQLHANDLE;

const
  SQL_SUCCESS = 0;
  SQL_NO_DATA = 100;
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;
  SQL_MAX_DSN_LENGTH = 32;
  SQL_MAX_OPTION_STRING_LENGTH = 256;

function SQLAllocEnv(var EnvironmentHandle: SQLHENV): SQLRETURN; stdcall; external 'odbc32.dll';

function SQLFreeEnv(EnvironmentHandle: SQLHENV): SQLRETURN; stdcall; external 'odbc32.dll';

function SQLDataSources(
  EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT;
  ServerName: PSQLCHAR;
  BufferLength1: SQLSMALLINT;
  var NameLength1: SQLSMALLINT;
  Description: PSQLCHAR;
  BufferLength2: SQLSMALLINT;
  var NameLength2: SQLSMALLINT): SQLRETURN; stdcall; external 'odbc32.dll';

procedure GetDSNList(sList : TStrings);
var
  EnvironmentHandle: SQLHANDLE;
  Buffer1: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  Buffer2: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
  Len1, Len2: SQLSMALLINT;
  ServerName {, Description} : string;
begin
  if SQLAllocEnv(EnvironmentHandle) = SQL_SUCCESS then
  try
    if SQLDataSources(EnvironmentHandle,
                      SQL_FETCH_FIRST,
                      Buffer1,
                      SizeOf(Buffer1),
                      Len1,
                      Buffer2,
                      SizeOf(Buffer2),
                      Len2) = SQL_SUCCESS then
      repeat
        SetString(ServerName, Buffer1, Len1);
        sList.Add(ServerName);
      until SQLDataSources(EnvironmentHandle,
                           SQL_FETCH_NEXT,
                           Buffer1,
                           SizeOf(Buffer1),
                           Len1,
                           Buffer2,
                           SizeOf(Buffer2),
                           Len2) = SQL_NO_DATA;
  finally
    SQLFreeEnv(EnvironmentHandle);
  end;
end;

procedure GetDSNList(sList : TStrings; const DSNType : string); overload;
var
  EnvironmentHandle: SQLHANDLE;
  Buffer1: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  Buffer2: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
  Len1, Len2: SQLSMALLINT;
  ServerName, Description: string;
begin
  if SQLAllocEnv(EnvironmentHandle) = SQL_SUCCESS then
  try
    if SQLDataSources(EnvironmentHandle,
                      SQL_FETCH_FIRST,
                      Buffer1,
                      SizeOf(Buffer1),
                      Len1,
                      Buffer2,
                      SizeOf(Buffer2),
                      Len2) = SQL_SUCCESS then
      repeat
        SetString(ServerName, Buffer1, Len1);
        SetString(Description, Buffer2, Len2);
        if CompareText(DSNType,Description) = 0 then
          sList.Add(ServerName);
      until SQLDataSources(EnvironmentHandle,
                           SQL_FETCH_NEXT,
                           Buffer1,
                           SizeOf(Buffer1),
                           Len1,
                           Buffer2,
                           SizeOf(Buffer2),
                           Len2) = SQL_NO_DATA;
  finally
    SQLFreeEnv(EnvironmentHandle);
  end;
end;

function CompareDSN(const Needle, Haystack : string):boolean;
begin
  // valid results are exact matches or starts with matches
  Result := CompareText(Needle, Haystack) = 0;
  if not Result then
    Result := CompareText(Needle, Copy(Haystack,1,Length(Needle))) = 0;
end;

procedure GetDSNList(sList : TStrings; DSNTypeList : array of String);
//procedure GetDSNList(sList : TStrings; DSNTypeList : TStrings);
var
  EnvironmentHandle: SQLHANDLE;
  Buffer1: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  Buffer2: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
  Len1, Len2: SQLSMALLINT;
  ServerName, Description: string;
  i : integer;
begin
  if SQLAllocEnv(EnvironmentHandle) = SQL_SUCCESS then
  try
    if SQLDataSources(EnvironmentHandle,
                      SQL_FETCH_FIRST,
                      Buffer1,
                      SizeOf(Buffer1),
                      Len1,
                      Buffer2,
                      SizeOf(Buffer2),
                      Len2) = SQL_SUCCESS then
      repeat
        SetString(ServerName, Buffer1, Len1);
        SetString(Description, Buffer2, Len2);
        for i := Low(DSNTypeList) to High(DSNTypeList) do begin
          // for DB2 v9 the description is appended by a potentially random string ie - DB2COPY1
//          if CompareText(DSNTypeList[i],Description) = 0 then begin
          if CompareDSN(DSNTypeList[i],Description) then begin
            sList.Add(ServerName);
            Break;
          end;
        end;
      until SQLDataSources(EnvironmentHandle,
                           SQL_FETCH_NEXT,
                           Buffer1,
                           SizeOf(Buffer1),
                           Len1,
                           Buffer2,
                           SizeOf(Buffer2),
                           Len2) = SQL_NO_DATA;
  finally
    SQLFreeEnv(EnvironmentHandle);
  end;
end;

end.
